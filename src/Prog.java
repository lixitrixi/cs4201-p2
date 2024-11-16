import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Prog {

    private class Unf {
        String id; // Con identifier
        List<Unf> fields; // Con fields
        Integer intval; // if int or bool

        public Unf(String id, Unf... a) {
            this.id = id;
            this.fields = Arrays.asList(a);
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
    }

    // Function declarations
%s

    public static void main(String[] args) {

        // Main program
%s
    }
}
