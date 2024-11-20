public class Prog {

    static class Uni {
        private String tag; // Con identifier
        private Uni[] fields; // Con fields
        private int intval; // if int or bool

        public Uni(String tag, Uni... a) {
            this.tag = tag;
            this.fields = a;
        }

        public Uni(int n) {
            this.tag = "$int";
            this.intval = n;
        }

        public Uni(boolean b) {
            this.tag = "$bool";
            this.intval = b ? 1 : 0;
        }

        public int toInt() {
            return this.intval;
        }

        public boolean toBool() {
            return this.intval != 0;
        }

        public String getTag() {
            return this.tag;
        }

        public Uni getArg(int i) {
            return this.fields[i];
        }

        public String toString() {
            switch (this.tag) {
                case "$int": return String.valueOf(this.toInt());
                case "$bool": return this.toBool() ? "true" : "false";
            }
            return this.tag + java.util.Arrays.toString(this.fields);
        }
    }

    // Generated function declarations
%s

    public static void main(String[] args) {

        // Main program
%s
    }
}
