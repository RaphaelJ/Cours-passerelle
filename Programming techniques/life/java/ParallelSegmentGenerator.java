/**
 * Computes the new state of a board using a set of threads which each processes
 * a bunch of consecutive cells.
 */
public class ParallelSegmentGenerator implements ILifeGenerator {
    private final int _n_threads;
    private final SegmentGenerator _gen;

    public ParallelSegmentGenerator(int n_threads, SegmentGenerator gen)
    {
        this._n_threads = n_threads;
        this._gen = gen;
    }

    public short[][] compute(Life origin) throws Exception
    {
        int w = origin.getW(), h = origin.getH();
        short[][] res = new short[h][w];
        int n = w * h;

        // Doesn't start more threads than the number of cells.
        int n_threads = Math.min(this._n_threads, n);
        int n_per_threads = n / n_threads;

        int i_start = 0;
        Thread[] tids = new Thread[n_threads - 1];
        for (int i = 0; i < n_threads - 1; i++) {
            int i_start_b = i_start;
            tids[i] = new Thread(new GeneratorTask(
                this._gen, res, origin, i_start, n_per_threads
            ));
            tids[i].start();
            i_start += n_per_threads;
        }

        // If the number of cells is not a multiple of the number of threads,
        // the last segment will contain the remainder of cells.
        // Doesn't execute the last segment in a new thread: uses the current
        // context.
        this._gen.compute(res, origin, i_start, n - i_start);

        // Waits for childs to finish.
        for (Thread tid : tids)
            tid.join();

        return res;
    }

    /**
      * Computes the new state of a subset of the board's cells.
      */
    public interface SegmentGenerator {
        /**
         * Computes the new state of a segment of the board of n cells starting
         * at start_i from origin in res.
         */
        public void compute(short[][] res, Life origin, int start_i, int n);
    }

    /**
     * Computes the new state of line-consecutive cells.
     */
    public static class LineSegmentGenerator implements SegmentGenerator
    {
        public void compute(short[][] res, Life origin, int start_i, int n)
        {
            int w = origin.getW();
            int x = start_i % w
              , y = start_i / w;

            for (int i = 0; i < n; i++) {
                res[y][x] = origin.nextCellState(x, y);

                x++;
                if (x >= w) {
                    x = 0;
                    y++;
                }
            }
        }
    }

    /**
     * Computes the new state of column-consecutive cells.
     */
    public static class ColumnSegmentGenerator implements SegmentGenerator
    {
        public void compute(short[][] res, Life origin, int start_i, int n)
        {
            int h = origin.getH();
            int x = start_i / h
              , y = start_i % h;

            for (int i = 0; i < n; i++) {
                res[y][x] = origin.nextCellState(x, y);

                y++;
                if (y >= h) {
                    y = 0;
                    x++;
                }
            }
        }
    }

    /**
      * Used to wrap a SegmentGenerator in a new thread.
      */
    private class GeneratorTask implements Runnable {
        private final SegmentGenerator _gen;
        private final short[][] _res;
        private final Life _origin;
        private final int _start_i;
        private final int _n;

        public GeneratorTask(
            SegmentGenerator gen, short[][] res, Life origin, int start_i,
            int n
        )
        {
            this._gen = gen;
            this._res = res;
            this._origin = origin;
            this._start_i = start_i;
            this._n = n;
        }

        public void run()
        {
            this._gen.compute(this._res, this._origin, this._start_i, this._n);
        }
    }
}