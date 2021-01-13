{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}

module AssemblyToCPSAssembly where

import qualified Assembly
import qualified CPSAssembly
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Tsil (Tsil)
import qualified Data.Tsil as Tsil
import qualified Literal
import qualified Name
import Protolude hiding (IntSet, IntMap, local, moduleName)

data ConverterState = ConverterState
  { _fresh :: !Int
  , _baseDefinitionName :: !Name.Lifted
  , _nextDefinitionName :: !Int
  , _finishDefinition :: !(CPSAssembly.BasicBlock -> (Assembly.Name, CPSAssembly.Definition))
  , _definitions :: Tsil (Assembly.Name, CPSAssembly.Definition)
  , _instructions :: Tsil CPSAssembly.Instruction
  , _stackPointer :: !Assembly.Local
  }

newtype Converter a = Converter { unConverter :: State ConverterState a }
  deriving (Functor, Applicative, Monad, MonadState ConverterState)

emitInstruction :: CPSAssembly.Instruction -> Converter ()
emitInstruction instruction =
  modify $ \s -> s { _instructions = _instructions s Tsil.:> instruction }

terminate :: CPSAssembly.Terminator -> Converter ()
terminate terminator =
  modify $ \s -> s
    { _definitions = _definitions s Tsil.:> _finishDefinition s (CPSAssembly.BasicBlock (toList $ _instructions s) terminator)
    , _instructions = mempty
    }

startDefinition :: (CPSAssembly.BasicBlock -> (Assembly.Name, CPSAssembly.Definition)) -> Converter ()
startDefinition finishDefinition =
  modify $ \s -> s { _finishDefinition = finishDefinition }

-------------------------------------------------------------------------------

wordSize :: Num a => a
wordSize = 8

freshFunctionName :: Converter Assembly.Name
freshFunctionName = do
  base <- gets _baseDefinitionName
  next <- gets _nextDefinitionName
  modify $ \s -> s { _nextDefinitionName = next + 1 }
  return $ Assembly.Name base next

freshLocal :: Assembly.NameSuggestion -> Converter Assembly.Local
freshLocal nameSuggestion = do
  fresh <- gets _fresh
  modify $ \s -> s { _fresh = fresh + 1 }
  pure $ Assembly.Local fresh nameSuggestion

push :: Assembly.Operand -> Converter ()
push operand = do
  stackPointer <- gets _stackPointer
  newStackPointer <- freshLocal "stack"
  emitInstruction $
    CPSAssembly.Sub
      newStackPointer
      (Assembly.LocalOperand stackPointer)
      (Assembly.Lit $ Literal.Integer wordSize)
  emitInstruction $
    CPSAssembly.Store
      (Assembly.LocalOperand newStackPointer)
      operand
  modify $ \s -> s { _stackPointer = newStackPointer }

pushLocal :: Assembly.Local -> Converter ()
pushLocal =
  push . Assembly.LocalOperand

pushLocals :: HashSet Assembly.Local -> Converter ()
pushLocals =
  mapM_ pushLocal . HashSet.toList

pop :: Assembly.Local -> Converter ()
pop local = do
  stackPointer <- gets _stackPointer
  emitInstruction $
    CPSAssembly.Load
      local
      (Assembly.LocalOperand stackPointer)
  newStackPointer <- freshLocal "stack"
  emitInstruction $
    CPSAssembly.Add
      newStackPointer
      (Assembly.LocalOperand stackPointer)
      (Assembly.Lit $ Literal.Integer wordSize)
  modify $ \s -> s { _stackPointer = newStackPointer }
  emitInstruction $ CPSAssembly.SetUndefined (Assembly.LocalOperand stackPointer) $ Assembly.Lit $ Literal.Integer wordSize

popLocals :: HashSet Assembly.Local -> Converter ()
popLocals =
  mapM_ pop . reverse . HashSet.toList

stackAllocate :: Assembly.Local -> Assembly.Operand -> Converter ()
stackAllocate newStackPointerDestination size = do
  stackPointer <- gets _stackPointer
  emitInstruction $ CPSAssembly.Sub newStackPointerDestination (Assembly.LocalOperand stackPointer) size
  modify $ \s -> s
    { _stackPointer = newStackPointerDestination
    }

stackDeallocate :: Assembly.Operand -> Converter ()
stackDeallocate size = do
  stackPointer <- gets _stackPointer
  newStackPointer <- freshLocal "stack"
  emitInstruction $ CPSAssembly.Add newStackPointer (Assembly.LocalOperand stackPointer) size
  modify $ \s -> s
    { _stackPointer = newStackPointer
    }
  emitInstruction $ CPSAssembly.SetUndefined (Assembly.LocalOperand stackPointer) size

-------------------------------------------------------------------------------

convertDefinition
  :: Int
  -> Name.Lifted
  -> Assembly.Definition Assembly.BasicBlock
  -> [(Assembly.Name, CPSAssembly.Definition)]
convertDefinition fresh name definition =
  toList $
    _definitions $
    flip execState ConverterState
      { _fresh = fresh + 1
      , _baseDefinitionName = name
      , _nextDefinitionName = 1
      , _finishDefinition = \ _ -> panic "convertDefinition"
      , _definitions = mempty
      , _instructions = mempty
      , _stackPointer = stackPointer
      } $ unConverter $ do
      case definition of
        Assembly.ConstantDefinition arguments basicBlock -> do
          modify $ \s  -> s
            { _finishDefinition =
              \basicBlock' ->
                ( Assembly.Name name 0
                , Assembly.ConstantDefinition (stackPointer : arguments) basicBlock'
                )
            }
          convertBasicBlock mempty $ Assembly.basicBlockWithOccurrences basicBlock

        Assembly.FunctionDefinition arguments basicBlock -> do
          modify $ \s  -> s
            { _finishDefinition =
              \basicBlock' ->
                ( Assembly.Name name 0
                , Assembly.FunctionDefinition (stackPointer : arguments) basicBlock'
                )
            }
          convertBasicBlock mempty $ Assembly.basicBlockWithOccurrences basicBlock
  where
    stackPointer =
      Assembly.Local fresh "stack"

convertBasicBlock :: HashSet Assembly.Local -> Assembly.BasicBlockWithOccurrences -> Converter ()
convertBasicBlock liveLocals basicBlock =
  case basicBlock of
    Assembly.Nil result -> do
      continuation <- freshLocal "continuation"
      pop continuation
      stackPointer <- gets _stackPointer
      terminate $ CPSAssembly.TailCall (Assembly.LocalOperand continuation) $
        case result of
          Assembly.VoidResult ->
            [Assembly.LocalOperand stackPointer]

          Assembly.Result operand ->
            [Assembly.LocalOperand stackPointer, operand]

    Assembly.Cons _ (Assembly.CallVoid function arguments) (Assembly.Nil result) -> do
      stackPointer <- gets _stackPointer
      terminate $ CPSAssembly.TailCall function $
        case result of
          Assembly.VoidResult ->
            Assembly.LocalOperand stackPointer : arguments

          Assembly.Result operand ->
            Assembly.LocalOperand stackPointer : operand : arguments

    Assembly.Cons _ instruction basicBlock' -> do
      convertInstruction (liveLocals <> Assembly.basicBlockOccurrences basicBlock') instruction
      convertBasicBlock liveLocals basicBlock'

convertInstruction
  :: HashSet Assembly.Local
  -> Assembly.Instruction Assembly.BasicBlockWithOccurrences
  -> Converter ()
convertInstruction liveLocals instr =
  case instr of
    Assembly.Copy o1 o2 o3 ->
      emitInstruction $ CPSAssembly.Copy o1 o2 o3

    Assembly.Call result function arguments -> do
      let
        liveLocals' =
          HashSet.delete result liveLocals
      pushLocals liveLocals'
      continuationFunctionName <- freshFunctionName
      push $ Assembly.GlobalFunction continuationFunctionName 1
      stackPointer <- gets _stackPointer
      terminate $ CPSAssembly.TailCall function $ Assembly.LocalOperand stackPointer : arguments
      startDefinition $ \basicBlock ->
        ( continuationFunctionName
        , Assembly.FunctionDefinition [stackPointer, result] basicBlock
        )
      popLocals liveLocals'

    Assembly.CallVoid function arguments -> do
      pushLocals liveLocals
      continuationFunctionName <- freshFunctionName
      push $ Assembly.GlobalFunction continuationFunctionName 0
      stackPointer <- gets _stackPointer
      terminate $ CPSAssembly.TailCall function $ Assembly.LocalOperand stackPointer : arguments
      startDefinition $ \basicBlock ->
        ( continuationFunctionName
        , Assembly.FunctionDefinition [stackPointer] basicBlock
        )
      popLocals liveLocals

    Assembly.Load l o ->
      emitInstruction $ CPSAssembly.Load l o

    Assembly.Store o1 o2 ->
      emitInstruction $ CPSAssembly.Store o1 o2

    Assembly.InitGlobal g o ->
      emitInstruction $ CPSAssembly.InitGlobal g o

    Assembly.Add l o1 o2 ->
      emitInstruction $ CPSAssembly.Add l o1 o2

    Assembly.Sub l o1 o2 ->
      emitInstruction $ CPSAssembly.Sub l o1 o2

    Assembly.StackAllocate l o ->
      stackAllocate l o

    Assembly.StackDeallocate o ->
      stackDeallocate o

    Assembly.HeapAllocate l o ->
      emitInstruction $ CPSAssembly.HeapAllocate l o

    Assembly.Switch scrutinee branches default_ -> do
      branches' <- forM branches $ \(i, basicBlock) -> do
        continuationFunctionName <- freshFunctionName
        let
          locals =
            liveLocals <> Assembly.basicBlockOccurrences basicBlock
          localsList =
            HashSet.toList locals
        pure
          ( i
          , continuationFunctionName
          , locals
          , localsList
          , basicBlock
          )
      stackPointer <- gets _stackPointer
      let
        branchTerminators =
          [ ( i
            , CPSAssembly.TailCall
              (Assembly.GlobalFunction continuationFunctionName $ HashSet.size locals)
              (Assembly.LocalOperand <$> stackPointer : localsList)
            )
          | (i, continuationFunctionName, locals, localsList, _) <- branches'
          ]
      defaultContinuationFunctionName <- freshFunctionName
      let
        defaultLocals =
          liveLocals <> Assembly.basicBlockOccurrences default_
        defaultLocalsList =
          HashSet.toList defaultLocals
      let defaultTerminator =
            CPSAssembly.TailCall
              (Assembly.GlobalFunction defaultContinuationFunctionName $ HashSet.size defaultLocals)
              (Assembly.LocalOperand <$> stackPointer : HashSet.toList defaultLocals)
      terminate $ CPSAssembly.Switch scrutinee branchTerminators defaultTerminator
      forM_ branches' $ \(_, continuationFunctionName, _locals, localsList, basicBlock) -> do
        startDefinition $ \basicBlock' ->
          ( continuationFunctionName
          , Assembly.FunctionDefinition (stackPointer : localsList) basicBlock'
          )
        convertBasicBlock liveLocals basicBlock
      startDefinition $ \basicBlock ->
        ( defaultContinuationFunctionName
        , Assembly.FunctionDefinition (stackPointer : defaultLocalsList) basicBlock
        )
      convertBasicBlock liveLocals default_
