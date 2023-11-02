import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe


type Instruction = (String, Int)
type Program = Map.Map Int Instruction
type ProgramCounter = Int
type Accumulator = Int
type ExecutedCommandsList = Set.Set Int

main :: IO ()
main = do  
    program <- Map.fromList . zip [0..] . map parseInstruction . lines <$> readFile "input.txt"
    putStrLn (show . filter (/=Nothing) . (map (flipAndTerminates program)) . Map.toList $ program)

flipAndTerminates :: Program -> (Int, Instruction) -> Maybe Int
flipAndTerminates p (pc, (inst, arg)) = terminatesWith Set.empty 0 0 (Map.insert pc (newInst, arg) p)
  where newInst = if inst == "nop" then "jmp" else if inst == "jmp" then "nop" else "acc"

accBeforeInfiniteLoop :: ExecutedCommandsList -> Accumulator -> ProgramCounter -> Program -> Int
accBeforeInfiniteLoop ecl acc pc p
  | instr == "nop" = accBeforeInfiniteLoop newECL acc (pc+1) p
  | instr == "acc" = accBeforeInfiniteLoop newECL (acc + arg) (pc+1) p
  | instr == "jmp" = if arg <= 0 && Set.member (pc + arg) newECL then acc else accBeforeInfiniteLoop newECL acc (pc+arg) p
  where (instr, arg) = p Map.! pc
        newECL = Set.insert pc ecl

terminatesWith :: ExecutedCommandsList -> Accumulator -> ProgramCounter -> Program -> Maybe Int
terminatesWith ecl acc pc p
  | Map.notMember (pc+1) p = Just acc
  | instr == "nop" = terminatesWith newECL acc (pc+1) p
  | instr == "acc" = terminatesWith newECL (acc + arg) (pc+1) p
  | instr == "jmp" = if arg <= 0 && Set.member (pc + arg) newECL then Nothing else terminatesWith newECL acc (pc+arg) p
  where (instr, arg) = p Map.! pc
        newECL = Set.insert pc ecl


highestInstructionReached :: ExecutedCommandsList -> Accumulator -> ProgramCounter -> Program -> Int
highestInstructionReached ecl acc pc p
  | instr == "nop" = highestInstructionReached newECL acc (pc+1) p
  | instr == "acc" = highestInstructionReached newECL (acc + arg) (pc+1) p
  | instr == "jmp" = if arg <= 0 && Set.member (pc + arg) newECL then maximum newECL else highestInstructionReached newECL acc (pc+arg) p
  where (instr, arg) = p Map.! pc
        newECL = Set.insert pc ecl

parseInstruction :: String -> Instruction
parseInstruction = (\[ins, value] -> (ins, read . filter (/='+') $ value)) . words
