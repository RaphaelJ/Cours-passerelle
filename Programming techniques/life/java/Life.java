import java.lang.StringBuilder;
import java.util.Calendar;

/**
 * Represents a state of the GoL board.
 * Provides a method to compute the next state of a cell.
 */
class Life {
    /**
     * Precomputes coordinates for each dimension of the board.
     * This increases the speed of the algorithm by 1.4x.
     * A such lookup table is kept between each generation of the GoL board
     * and is used by the nextCellState() method.
     */
    private class IndexLookupTable {
        public final int[] x1s;
        public final int[] x2s;
        public final int[] y1s;
        public final int[] y2s;

        public IndexLookupTable(int w, int h)
        {
            this.x1s = new int[w];
            this.x1s[0] = w - 1;
            for (int i = 1; i < w; i++)
                this.x1s[i] = i - 1;

            this.x2s = new int[w];
            this.x2s[w - 1] = 0;
            for (int i = 0; i < w - 1; i++)
                this.x2s[i] = i + 1;

            this.y1s = new int[h];
            this.y1s[0] = h - 1;
            for (int i = 1; i < h; i++)
                this.y1s[i] = i - 1;

            this.y2s = new int[h];
            this.y2s[h - 1] = 0;
            for (int i = 0; i < h - 1; i++)
                this.y2s[i] = i + 1;
        }
    }

    private final int _w;
    private final int _h;
    private final short[][] _board;
    private final IndexLookupTable _index;

    public Life(short[][] board)
    {
        this._h = board.length;
        this._w = this._h == 0 ? 0 : board[0].length;

        this._board = board;
        this._index = new IndexLookupTable(this._w, this._h);
    }

    private Life(short[][] board, IndexLookupTable index)
    {
        this._h = board.length;
        this._w = this._h == 0 ? 0 : board[0].length;

        this._board = board;
        this._index = index;
    }

    public Life nextState(ILifeGenerator gen) throws Exception
    {
        short[][] res = gen.compute(this);

        return new Life(res, this._index);
    }

    /**
     * Computes the next state of a cell. Use the coordinates lookup table
     * to avoid multiple computations of the same neighbours indexes.
     */
    public short nextCellState(int x, int y)
    {
        int x1 = this._index.x1s[x],
            x2 = this._index.x1s[x],
            y1 = this._index.x1s[y],
            y2 = this._index.x1s[y];

        // short[] neighbours = new short[] {
        //      this._board[y1][x1], this._board[y1][x ], this._board[y1][x2],
        //      this._board[y ][x1],                      this._board[y ][x2],
        //      this._board[y2][x1], this._board[y2][x ], this._board[y2][x2]
        // };
        //
        // int n = 0;
        // for (short neighbour : neighbours) {
        //     if (neighbour)
        //         n++;
        // }

        // Unrolling the previous loop and using an array of shorts gived me
        // 5x speedup.

        int n = this._board[y1][x1] + this._board[y1][x ] + this._board[y1][x2]
              + this._board[y ][x1] +                       this._board[y ][x2]
              + this._board[y2][x1] + this._board[y2][x ] + this._board[y2][x2];

        if  (n == 3 || (n == 2 && this._board[y][x] == 1))
            return 1;
        else
            return 0;
    }

    public String toString()
    {
        StringBuilder str = new StringBuilder();

        for (short[] line : this._board) {
            for (short cell : line) {
                if (cell == 1)
                    str.append('X');
                else
                    str.append(' ');
            }
            str.append('\n');
        }

        return str.toString();
    }

    // ===== Getters ====

    public int getW()
    {
        return this._w;
    }

    public int getH()
    {
        return this._h;
    }

    // ===== Static methods =====

    public static void main(String[] args) throws Exception
    {
        if (args.length != 2)
            System.out.println("USAGE: java Life <board size> <# of threads>");
        else {
            int size        = Integer.parseInt(args[0]);
            int num_threads = Integer.parseInt(args[1]);

            Life origin = new Life(randomBoard(size, size));

            testGenerator(origin, new SequentialGenerator());
            testGenerator(origin, new ParallelLineGenerator(num_threads));
        }
    }

    private static void testGenerator(Life origin, ILifeGenerator gen)
        throws Exception
    {
        long start = Calendar.getInstance().getTimeInMillis();

        for (int i = 0; i < 100; i++)
            origin = origin.nextState(gen);

        long stop = Calendar.getInstance().getTimeInMillis();
        System.out.println(
            "Execution time: " + ((double) (stop - start) / 100)
        );
    }

    public static short[][] randomBoard(int w, int h)
    {
        short[][] board = new short[h][w];

        for (int y = 0; y < h; y++) {
            for (int x = 0; x < w; x++) {
                if (Math.random() > 0.5)
                    board[y][x] = 1;
                else
                    board[y][x] = 0;
            }
        }

        return board;
    }
}
