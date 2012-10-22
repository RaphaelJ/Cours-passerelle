import Control.Monad.State
import Data.Arrow
import Data.Map
import Data.Sequence
import Data.Word

type Labels = Map String Int
type Program = Sequence Word

data AssemblerState = AssemblerState {
      asLabels :: Labels, asProgram :: Program
    } deriving (Show, Read)

type Assembler = State AssemblerState

runAssembler :: Assembler a -> (a, Program)
runAssembler = (second program) . runState

execAssembler :: Assembler a -> Program
execAssembler = snd . runAssembler

-- | Registers
newtype Register = Register Word
r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r18
, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31
, bp :: Register
[ r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r18
, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31 ] =
    map Register [0..30]
bp = r27

putWord :: Word -> Assembler ()
putWord w = do
    state <- get
    put $ state { asProgram = asProgram state |> w }

putWords :: [Word] -> Assembler ()
putWords []     = return ()
putWords (w:ws) = putWord w >> putWords ws

add :: Register -> Register -> Register -> Assembler ()
add ra rb rc = putWords [0x20, ra, rb, rc]

