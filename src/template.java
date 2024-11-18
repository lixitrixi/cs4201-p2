public class Prog {

    static class Unf {
        String id; // Con identifier
        Unf[] fields; // Con fields
        Integer intval; // if int or bool

        public Unf(String id, Unf... a) {
            this.id = id;
            this.fields = a;
        }

        public Unf(int n) {
            this.id = "__int";
            this.intval = n;
        }

        public Unf(boolean b) {
            this.id = "__bool";
            this.intval = b ? 1 : 0;
        }

        public int toInt() {
            return this.intval;
        }

        public boolean toBool() {
            return this.intval != 0;
        }

        public String toString() {
            switch (this.id) {
                case "__int": return String.valueOf(this.toInt());
                case "__bool": return this.toBool() ? "true" : "false";
            }
            return this.id + java.util.Arrays.toString(this.fields);
        }
    }

    // Generated function declarations
%s

    public static void main(String[] args) {

        // Main program
%s
    }
}
