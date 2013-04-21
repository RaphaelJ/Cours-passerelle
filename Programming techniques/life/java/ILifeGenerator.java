/**
 * Represents the different kind of GoL generators.
 * This is a common interface for sequential and parallel generators.
 */
public interface ILifeGenerator {
    /**
     * Computes the next state of the board using the original board.
     */
    public short[][] compute(Life origin) throws Exception;
}
