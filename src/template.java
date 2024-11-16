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

        public int asi() {
            return this.intval;
        }

        public boolean asb() {
            return this.intval != 0;
        }

        public String toString() {
            switch (this.id) {
                case "__int": return String.valueOf(this.asi());
                case "__bool": return this.asb() ? "true" : "false";
            }
            return this.id + "(" + this.fields.toString() + ")";
        }
    }

    // Generated function declarations
%s

    public static void main(String[] args) {

        // Main program
%s
    }
}
