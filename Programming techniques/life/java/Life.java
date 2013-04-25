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
        public final int[] prev;
        public final int[] next;

        public IndexLookupTable(int size)
        {
            this.prev = new int[size];
            this.prev[0] = size - 1;
            for (int i = 1; i < size; i++)
                this.prev[i] = i - 1;

            this.next = new int[size];
            this.next[size - 1] = 0;
            for (int i = 0; i < size - 1; i++)
                this.next[i] = i + 1;
        }
    }

    private final int _size;
    private final byte[][] _board;
    private final IndexLookupTable _index;

    public Life(byte[][] board)
    {
        this._size = board.length;

        this._board = board;
        this._index = new IndexLookupTable(this._size);
    }

    private Life(byte[][] board, IndexLookupTable index)
    {
        this._size = board.length;

        this._board = board;
        this._index = index;
    }

    public Life nextState(ILifeGenerator gen) throws Exception
    {
        byte[][] res = gen.compute(this);

        return new Life(res, this._index);
    }

    /**
     * Computes the next state of a cell. Use the coordinates lookup table
     * to avoid multiple computations of the same neighbours indexes.
     */
    public byte nextCellState(int x, int y)
    {
        int x1 = this._index.prev[x]
          , x2 = this._index.next[x]
          , y1 = this._index.prev[y]
          , y2 = this._index.next[y];

        // boolean[] neighbours = new boolean[] {
        //      this._board[y1][x1], this._board[y1][x ], this._board[y1][x2],
        //      this._board[y ][x1],                      this._board[y ][x2],
        //      this._board[y2][x1], this._board[y2][x ], this._board[y2][x2]
        // };
        //
        // int n = 0;
        // for (boolean neighbour : neighbours) {
        //     if (neighbour)
        //         n++;
        // }

        // Unrolling the previous loop and using an array of bytes gived me
        // 5x speedup.

        int n =
            this._board[y1][x1] + this._board[y1][x ] + this._board[y1][x2]
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

        for (byte[] line : this._board) {
            for (byte cell : line) {
                if (cell == 1)
                    str.append('X');
                else
                    str.append(' ');
            }
            str.append('\n');
        }

        return str.toString();
    }

    /* Getters */

    public int getSize()
    {
        return this._size;
    }

    /* Static methods */

    public static void main(String[] args) throws Exception
    {
        if (args.length != 2)
            System.out.println("USAGE: java Life <board size> <# of threads>");
        else {
            int size        = Integer.parseInt(args[0]);
            int n_threads = Integer.parseInt(args[1]);

            byte[][] board = randomBoard(size);

            if (n_threads == 1)
                testGenerator(
                    "sequential generator", new Life(board),
                    new SequentialGenerator()
                );

            if (size < 50)
                testGenerator(
                    "parallel cell generator", new Life(board),
                    new ParallelCellGenerator()
                );

            testGenerator(
                "parallel line generator", new Life(board),
                new ParallelSegmentGenerator(
                    n_threads,
                    new ParallelSegmentGenerator.LineSegmentGenerator()
                )
            );
            testGenerator(
                "parallel column generator", new Life(board),
                new ParallelSegmentGenerator(
                    n_threads,
                    new ParallelSegmentGenerator.ColumnSegmentGenerator()
                )
            );

            // Only for square number of threads.
            int n_threads_sqrt = (int) Math.sqrt(n_threads);
            if (n_threads_sqrt * n_threads_sqrt == n_threads)
                testGenerator(
                    "block generator", new Life(board),
                    new ParallelBlockGenerator(n_threads)
                );
        }
    }

    /**
     * Executes 100 times a generator and print the average time per sample.
     */
    private static void testGenerator(
        String test_name, Life origin, ILifeGenerator gen
    ) throws Exception
    {
        System.out.print("Testing " + test_name + " ... ");

        long start = Calendar.getInstance().getTimeInMillis();

        for (int i = 0; i < 100; i++)
            origin = origin.nextState(gen);

        long stop = Calendar.getInstance().getTimeInMillis();
        System.out.println(
            "Execution time: " + ((double) (stop - start) / 100) + " ms"
        );
    }

    /**
     * Generates a random board with 1 and 0 values.
     */
    public static byte[][] randomBoard(int size)
    {
        byte[][] board = new byte[size][size];

        for (int y = 0; y < size; y++) {
            for (int x = 0; x < size; x++) {
                if (Math.random() > 0.5)
                    board[y][x] = 1;
                else
                    board[y][x] = 0;
            }
        }

        return board;
    }
}
