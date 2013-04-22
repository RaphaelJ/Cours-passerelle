public class SequentialGenerator implements ILifeGenerator {
    public byte[][] compute(Life origin)
    {
        int size = origin.getSize();

        byte[][] res = new byte[size][size];

        for (int y = 0; y < size; y++)
            for (int x = 0; x < size; x++)
                res[y][x] = origin.nextCellState(x, y);

        return res;
    }
}
