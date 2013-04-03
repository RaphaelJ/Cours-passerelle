public class SequentialGenerator implements ILifeGenerator {
    public short[][] compute(Life origin)
    {
        int w = origin.getW(), h = origin.getH();

        short[][] res = new short[h][w];

        for (int y = 0; y < h; y++) {
            for (int x = 0; x < w; x++)
                res[y][x] = origin.nextCellState(x, y);
        }

        return res;
    }
}
