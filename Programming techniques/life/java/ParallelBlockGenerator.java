public class ParallelBlockGenerator implements ILifeGenerator {
    private final int _n_threads;

    public ParallelBlockGenerator(int n_threads)
    {
        this._n_threads = n_threads;
    }

    public byte[][] compute(Life origin) throws Exception
    {
        int size = origin.getSize();
        byte[][] res = new byte[size][size];

        int n_threads_side = (int) Math.sqrt(this._n_threads);

        if (n_threads_side * n_threads_side != this._n_threads)
            throw new Exception(
                "The number of threads must be a square."
            );
        else if (size % n_threads_side != 0)
            throw new Exception(
                "The number of cells must a multiple of the number of threads."
            );

        if (n_threads_side > 1) {

            int block_size = size / n_threads_side;

            Thread[][] tids = new Thread[n_threads_side][n_threads_side];
            int start_y = 0;
            for (int i = 0; i < n_threads_side; i++) {
                    int start_x = 0;
                for (int j = 0; j < n_threads_side; j++) {
                    tids[i][j] = new Thread(new GeneratorTask(
                        res, origin, start_x, start_y, block_size
                    ));
                    tids[i][j].start();

                    start_x += block_size;
                }
                start_y += block_size;
            }

            // Waits for childs to finish.
            for (Thread[] line : tids)
                for (Thread tid : line)
                    tid.join();
        } else // Sequential generation.
            new GeneratorTask(res, origin, 0, 0, size).run();

        return res;
    }

    /**
     * Computes the next state of a square block of cells.
     */
    private class GeneratorTask implements Runnable {
        private final byte[][] _res;
        private final Life _origin;
        private final int _start_x;
        private final int _start_y;
        private final int _block_size;

        public GeneratorTask(
            byte[][] res, Life origin, int start_x, int start_y, int block_size
        )
        {
            this._res = res;
            this._origin = origin;
            this._start_x = start_x;
            this._start_y = start_y;
            this._block_size = block_size;
        }

        public void run()
        {
            int max_x = this._start_x + this._block_size
              , max_y = this._start_y + this._block_size;

            for (int y = this._start_y; y < max_y; y++)
                for (int x = this._start_x; x < max_x; x++)
                    this._res[y][x] = this._origin.nextCellState(x, y);
        }
    }
}