// Raphael Javaux - 2012

import java.text.ParseException;
import java.util.*;

public class Formula implements Cloneable {
    Operation _syntaxTree; // Racine de l'abre syntaxique de la formule
    
    public static void main(String[] args) throws Exception
    {
        Formula f = new Formula("add add mul p q 5 0");
        System.out.println(f.toString());
        Formula g = new Formula("div 7 q");
        f.sub(g);
        System.out.println(f.toString());
        f.eval("q", 2);
        System.out.println(f.toString());
        f.eval("p", 4);
        System.out.println(f.toString());
    }
    
    public Formula(String expr) throws Exception 
    {
        this._syntaxTree = Operation.parse(expr);
    }
    
    public Operation syntaxTree() { return this._syntaxTree; }
    
    // Puisque les opérations de manipulation de l'arbre syntaxique sont 
    // implicitement thread-safe, car immuables (voir commentaire plus bas),
    // il suffit de déclarer quelques verrous avec synchronized sur l'unique 
    // partie mutable du travail (_syntaxTree) pour obtenir une interface 
    // complètement concurrente !
    
    public synchronized void eval(String variable, int value) throws Exception 
    {
        this._syntaxTree = this._syntaxTree.eval(variable, value);
        this._syntaxTree = this._syntaxTree.simplify(true);
    }
    
    public synchronized void add(Formula g) throws Exception 
    {
        this._syntaxTree = new Addition(this._syntaxTree, g.syntaxTree());
        this._syntaxTree = this._syntaxTree.simplify(false);
    }
    
    public synchronized void sub(Formula g) throws Exception 
    {
        this._syntaxTree = new Subtraction(this._syntaxTree, g.syntaxTree());
        this._syntaxTree = this._syntaxTree.simplify(false);
    }
    
    public synchronized void mul(Formula g) throws Exception 
    {
        this._syntaxTree = new Multiplication(this._syntaxTree, g.syntaxTree());
        this._syntaxTree = this._syntaxTree.simplify(false);
    }

    public synchronized void div(Formula g) throws Exception 
    {
        this._syntaxTree = new Division(this._syntaxTree, g.syntaxTree());
        this._syntaxTree = this._syntaxTree.simplify(false);
    }
    
    public String toString() { return this._syntaxTree.toString(); }
}

class DivisionByZeroException extends Exception {
    public DivisionByZeroException() { super("Division by zero"); }
}

// Classe générique utilisée pour un simple groupement de deux valeurs
// hétérogènes (par exemple, pour retourner deux valeurs d'une fonction).
class Tuple2<T, U> {
    final T _fst;
    final U _snd;
    
    public Tuple2(T fst, U snd)
    {
        this._fst = fst;
        this._snd = snd;
    }
    
    public T fst() { return this._fst; }
    public U snd() { return this._snd; }
}

// Classe abstraite dont héritent tous les noeuds de l'arbre syntaxique.
// simplify() tente de retourner une simplification de l'opération. Dans le
// cas des méthodes add(), sub() ... de la classe Formula, il n'est pas
// nécessaire de simplifier l'ensemble de l'arbre syntaxique mais uniquement la
// racine (en utilisant le paramètre recursive de la méthode).
// eval() retourne l'opération une fois que la définition de la variable a été 
// appliquée. eval() doit effectuer la propagation de la variable à ses noeuds
// enfants, récursivement. eval() n'effectue pas de simplification.
// simplify() et eval() ne doivent en aucun cas modifier le neoud auquel elles
// sont appliquées mais retourner le nouvel état du noeud (éventuellement le 
// noeud lui-même s'il n'a pas été modifié). 
// Toutes les classes d'opération sont des types immuables (càd que tous leurs 
// champs sont final) pour garantir la non-modification des champs. Ceci 
// engendre une légère surcharge au niveau du garbage collector (à cause de la 
// copie de toutes les données d'un noeud lors de la moindre simplification d'un 
// champ) mais permet de partager un même arbre syntaxique entre plusieurs
// formules, d'éviter tout effet de bord, de rendre le code thread-safe (sans le
// moindre verrou) et de simplifier considérablement la programmation des 
// simplifications.
abstract class Operation {
    public abstract Operation simplify(boolean recursive) throws Exception;
    public abstract Operation eval(String variable, int value);
    
    // Méthodes statiques pour le parsing des formules :
    
    // Retourne un arbre syntaxique, resultat de l'analyse récursive du texte 
    // de l'expression. Cette fonction délègue l'analyse à la seconde fonction
    // parse(). L'arbre retourné est simplifié avant d'être retourné.
    public static Operation parse(String expr) throws Exception
    {
        String[] vectExpr = Operation.splitBySpaces(expr);
        Tuple2<Operation, Integer> result = Operation.parse(vectExpr, 0);
        
        // L'expression ne se termine pas au dernier opérateur
        if (result.snd().intValue() != vectExpr.length) {
            throw new ParseException(
                "Expression not correctly terminated", result.snd().intValue()
            );
        }
        
        return result.fst().simplify(true);
    }
    
    // Retourne un arbre syntaxique, resultat de l'analyse récursive des mots 
    // de l'expression, ainsi que le nouvel indice du curseur dans l'expression
    // pour le mot suivant.
    public static Tuple2<Operation, Integer> parse(String[] expr, int cursor)
        throws ParseException
    {
        if (cursor >= expr.length)
            throw new ParseException("Unfinished or empty expression", cursor);
            
        String word = expr[cursor];
        
        if (Character.isDigit(word.charAt(0)) || word.charAt(0) == '-') {
            return new Tuple2<Operation, Integer>(
                new Constant(Integer.parseInt(word)), cursor + 1
            );
        } else if (word.equals("add")) {
            Tuple2<Operation[], Integer> subs = Operation.parseBinaryOp(
                expr, cursor + 1
            );
            return new Tuple2<Operation, Integer>(
                new Addition(subs.fst()[0], subs.fst()[1]), subs.snd()
            );
        } else if (word.equals("sub")) {
            Tuple2<Operation[], Integer> subs = Operation.parseBinaryOp(
                expr, cursor + 1
            );
            return new Tuple2<Operation, Integer>(
                new Subtraction(subs.fst()[0], subs.fst()[1]), subs.snd()
            );
        } else if (word.equals("mul")) {
            Tuple2<Operation[], Integer> subs = Operation.parseBinaryOp(
                expr, cursor + 1
            );
            return new Tuple2<Operation, Integer>(
                new Multiplication(subs.fst()[0], subs.fst()[1]), subs.snd()
            );
        } else if (word.equals("div")) {
            Tuple2<Operation[], Integer> subs = Operation.parseBinaryOp(
                expr, cursor + 1
            );
            return new Tuple2<Operation, Integer>(
                new Division(subs.fst()[0], subs.fst()[1]), subs.snd()
            );
        } else { // Variable
            return new Tuple2<Operation, Integer>(
                new Variable(word), cursor + 1
            );
        }
    }
    
    // Retourne un vecteur contenant tous les mots séparés par un ou plusieurs
    // caractères d'espacement.
    private static String[] splitBySpaces(String expr)
    {
        LinkedList<String> words = new LinkedList<String>();
        
        int start = -1; // -1 si pas dans un mot
        
        for (int cursor = 0; cursor < expr.length(); cursor++) {
            if (start != -1 && Character.isWhitespace(expr.charAt(cursor))) {
                // Fin d'un mot
                words.addLast(expr.substring(start, cursor));
                start = -1;
            } else if (
                start == -1 && !Character.isWhitespace(expr.charAt(cursor))
            ) {
                // Début d'un mots
                start = cursor;
            }
        }
        
        if (start != -1) // Dernier mot
            words.addLast(expr.substring(start, expr.length()));
        
        return words.toArray(new String[0]);
    }
    
    // Parse les deux sous-opérations d'un opérateur binaire (add, mul ...).
    // Retourne les deux sous-opérations ainsi que le nouveau curseur.
    private static Tuple2<Operation[], Integer> parseBinaryOp(
        String[] expr, int cursor
    ) throws ParseException
    {
        Tuple2<Operation, Integer> op1 = Operation.parse(expr, cursor);
        Tuple2<Operation, Integer> op2 = Operation.parse(
            expr, op1.snd().intValue()
        );
        
        return new Tuple2<Operation[], Integer>(
            new Operation[] { op1.fst(), op2.fst() }, op2.snd().intValue()
        );
    }
}

// Classe contenant une constante entière.
class Constant extends Operation {
    final int _value;
    
    public Constant(int value) { this._value = value; }
    
    public int value() { return this._value; }
    
    public Operation simplify(boolean recursive) throws Exception
    {
        return this;
    }
    
    public Operation eval(String variable, int value) { return this; }
    
    public String toString() { return Integer.toString(this._value); }
}

// Classe contenant une variable. Si la variable est initialisée par eval, une
// constante est retournée.
class Variable extends Operation {
    final String _name;
    
    public Variable(String name) { this._name = name; }
    
    public Operation simplify(boolean recursive) throws Exception
    {
        return this;
    }
    
    public Operation eval(String variable, int value)
    {    
        if (this._name.equals(variable)) {
            return new Constant(value);
        } else
            return this;
    }
    
    public String toString() { return this._name; }
}

// Classe abstraite dont vont dériver les opérations qui s'effectuent sur deux
// opérandes (addition, soustraction, multiplication et division).
abstract class BinaryOp extends Operation {
    final Operation _op1;
    final Operation _op2;
    
    public BinaryOp(Operation op1, Operation op2)
    {
        this._op1 = op1; this._op2 = op2;
    }
    
    public Operation op1() { return this._op1; }
    public Operation op2() { return this._op2; }
    
    // Retourne le nom de l'opération (add, sub, ...) afin d'afficher la formule
    // dans toString().
    public abstract String opName();
    
    public String toString() 
    {
        return this.opName() + " " + this.op1().toString() 
             + " " + this.op2().toString();
    }
}

// Effectue une addition. Applique les simplifications lors de l'évaluation.
class Addition extends BinaryOp {
    public Addition(Operation op1, Operation op2) { super(op1, op2); }
    
    public Operation simplify(boolean recursive) throws Exception
    {
        Operation op1;
        Operation op2;
        if (recursive) {
            op1 = this._op1.simplify(true);
            op2 = this._op2.simplify(true);
        } else {
            op1 = this._op1;
            op2 = this._op2;
        }
        
        if (op1 instanceof Constant) {
            int op1_val = ((Constant) op1).value();
            if (op2 instanceof Constant) // Evaluation complète
                return new Constant(op1_val + ((Constant) op2).value());
            else if (op1_val == 0) // Première opérande nulle
                return op2;
            else
                return new Addition(op1, op2);
        } else if (op2 instanceof Constant && ((Constant) op2).value() == 0) {
            // Seconde opérande nulle
            return op1; 
        } else {
            return new Addition(op1, op2);
        }
    }
    
    public Operation eval(String variable, int value)
    {
        return new Addition(
            this._op1.eval(variable, value), this._op2.eval(variable, value)
        );
    }
    
    public String opName() { return "add"; }
}

// Effectue une soustraction. Applique les simplifications lors de l'évaluation.
class Subtraction extends BinaryOp {
    public Subtraction(Operation op1, Operation op2) { super(op1, op2); }
    
    public Operation simplify(boolean recursive) throws Exception
    {
        Operation op1;
        Operation op2;
        if (recursive) {
            op1 = this._op1.simplify(true);
            op2 = this._op2.simplify(true);
        } else {
            op1 = this._op1;
            op2 = this._op2;
        }
        
        if (op1 instanceof Constant && op2 instanceof Constant) {
            // Les deux opérandes sont constantes, évaluation complète
            return new Constant(
                ((Constant) op1).value() - ((Constant) op2).value()
            );
        } else if (op2 instanceof Constant && ((Constant) op2).value() == 0) {
            // Seconde opérande nulle
            return op1; 
        } else {
            return new Subtraction(op1, op2);
        }
    }
    
    public Operation eval(String variable, int value)
    {
        return new Subtraction(
            this._op1.eval(variable, value), this._op2.eval(variable, value)
        );
    }
    
    public String opName() { return "sub"; }
}

// Effectue une multiplication. Applique les simplifications lors de
// l'évaluation.
class Multiplication extends BinaryOp {
    public Multiplication(Operation op1, Operation op2) { super(op1, op2); }
    
    public Operation simplify(boolean recursive) throws Exception
    {
        Operation op1;
        Operation op2;
        if (recursive) {
            op1 = this._op1.simplify(true);
            op2 = this._op2.simplify(true);
        } else {
            op1 = this._op1;
            op2 = this._op2;
        }
        
        if (op1 instanceof Constant) {
            int op1_val = ((Constant) op1).value();
            if (op2 instanceof Constant) // Evaluation complète
                return new Constant(op1_val * ((Constant) op2).value());
            else if (op1_val == 0) // Première opérande nulle, résultat nul
                return op1;
            else if (op1_val == 1) // Première opérande à 1
                return op2;
            else 
                return new Multiplication(op1, op2);
        } else if (op2 instanceof Constant) {
            int op2_val = ((Constant) op2).value();
            if (op2_val == 0) // Seconde opérande nulle, résultat nul
                return op2;
            else if (op2_val == 1) // Seconde opérande à 1
                return op1;
            else
                return new Multiplication(op1, op2);
        } else { // Aucune opérande constante
            return new Multiplication(op1, op2);
        }
    }
    
    public Operation eval(String variable, int value)
    {
        return new Multiplication(
            this._op1.eval(variable, value), this._op2.eval(variable, value)
        );
    }
    
    public String opName() { return "mul"; }
}

// Effectue une division. Applique les simplifications lors de l'évaluation.
class Division extends BinaryOp {
    public Division(Operation op1, Operation op2) { super(op1, op2); }
    
    public Operation simplify(boolean recursive) throws Exception
    {
        Operation op1;
        Operation op2;
        if (recursive) {
            op1 = this._op1.simplify(true);
            op2 = this._op2.simplify(true);
        } else {
            op1 = this._op1;
            op2 = this._op2;
        }
        
        if (op1 instanceof Constant) {
            int op1_val = ((Constant) op1).value();
            if (op2 instanceof Constant) {
                // Les deux opérandes sont constantes, évaluation complète si
                // le dénominateur est différent de zéro.
                int op2_val = ((Constant) op2).value();
                if (op2_val == 0)
                    throw new DivisionByZeroException();
                
                return new Constant(op1_val / op2_val);
            } else if (op1_val == 0) {
                // Le numérateur est nul, retourne une constant nulle
                return new Constant(0);
            } else { // Pas de simplification 
                return new Division(op1, op2);
            }
        } else if (op2 instanceof Constant && ((Constant) op2).value() == 1) {
            // Dénominateur à 1, suppression du dénominateur
            return op1;
        } else {
            return new Division(op1, op2);
        }
    }
    
    public Operation eval(String variable, int value)
    {
        return new Division(
            this._op1.eval(variable, value), this._op2.eval(variable, value)
        );
    }
    
    public String opName() { return "div"; }
}