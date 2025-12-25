#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

#define MAX_EXPR 256
#define MAX_STACK 100
#define PI 3.14159265358979
#define GRAVITY 9.80665

// ==================== STRUCTURES ====================

typedef struct {
    double items[MAX_STACK];
    int top;
} NumStack;

typedef struct {
    char items[MAX_STACK];
    int top;
} OpStack;

// ==================== UTILITY FUNCTIONS ====================

void clearBuffer() {
    int c;
    while ((c = getchar()) != '\n' && c != EOF);
}

// ==================== STACK OPERATIONS ====================

void initNumStack(NumStack *s) { s->top = -1; }
void initOpStack(OpStack *s) { s->top = -1; }

void pushNum(NumStack *s, double val) {
    if (s->top < MAX_STACK - 1) s->items[++(s->top)] = val;
}

double popNum(NumStack *s) {
    if (s->top >= 0) return s->items[(s->top)--];
    return 0;
}

void pushOp(OpStack *s, char op) {
    if (s->top < MAX_STACK - 1) s->items[++(s->top)] = op;
}

char popOp(OpStack *s) {
    if (s->top >= 0) return s->items[(s->top)--];
    return '\0';
}

char peekOp(OpStack *s) {
    if (s->top >= 0) return s->items[s->top];
    return '\0';
}

int isOpStackEmpty(OpStack *s) { return s->top == -1; }

// ==================== EXPRESSION CALCULATOR HELPERS ====================

int getPrecedence(char op) {
    if (op == '+' || op == '-') return 1;
    if (op == '*' || op == '/') return 2;
    if (op == '^') return 3;
    return 0;
}

double applyOp(double a, double b, char op) {
    switch (op) {
        case '+': return a + b;
        case '-': return a - b;
        case '*': return a * b;
        case '/': return (b != 0) ? a / b : 0;
        case '^': return pow(a, b);
    }
    return 0;
}

double parseNum(char *expr, int *i) {
    double result = 0, decimal = 0.1;
    
    while (isdigit(expr[*i])) {
        result = result * 10 + (expr[*i] - '0');
        (*i)++;
    }
    
    if (expr[*i] == '.') {
        (*i)++;
        while (isdigit(expr[*i])) {
            result += (expr[*i] - '0') * decimal;
            decimal *= 0.1;
            (*i)++;
        }
    }
    return result;
}

double evaluateExpr(char *expr);

double parseFunc(char *expr, int *i) {
    char func[10];
    int f = 0, paren = 0;
    char arg[MAX_EXPR];
    int a = 0;
    double val;
    
    while (isalpha(expr[*i])) func[f++] = expr[(*i)++];
    func[f] = '\0';
    
    if (expr[*i] == '(') {
        (*i)++;
        paren = 1;
        while (paren > 0) {
            if (expr[*i] == '(') paren++;
            else if (expr[*i] == ')') paren--;
            if (paren > 0) arg[a++] = expr[*i];
            (*i)++;
        }
        arg[a] = '\0';
        val = evaluateExpr(arg);
        
        if (strcmp(func, "sqrt") == 0) return sqrt(val);
        if (strcmp(func, "log") == 0) return log10(val);
        if (strcmp(func, "sin") == 0) return sin(val * PI / 180);
        if (strcmp(func, "cos") == 0) return cos(val * PI / 180);
        if (strcmp(func, "tan") == 0) return tan(val * PI / 180);
    }
    return 0;
}

double evaluateExpr(char *expr) {
    NumStack nums;
    OpStack ops;
    int i = 0, len = strlen(expr);
    
    initNumStack(&nums);
    initOpStack(&ops);
    
    while (i < len) {
        if (isspace(expr[i])) { i++; continue; }
        
        if (isdigit(expr[i]) || (expr[i] == '.' && isdigit(expr[i+1]))) {
            pushNum(&nums, parseNum(expr, &i));
        }
        else if (isalpha(expr[i])) {
            pushNum(&nums, parseFunc(expr, &i));
        }
        else if (expr[i] == '(') {
            pushOp(&ops, '(');
            i++;
        }
        else if (expr[i] == ')') {
            while (!isOpStackEmpty(&ops) && peekOp(&ops) != '(') {
                double b = popNum(&nums), a = popNum(&nums);
                pushNum(&nums, applyOp(a, b, popOp(&ops)));
            }
            popOp(&ops);
            i++;
        }
        else if (strchr("+-*/^", expr[i])) {
            if (expr[i] == '-' && (i == 0 || strchr("(+-*/^", expr[i-1]))) {
                i++;
                pushNum(&nums, -parseNum(expr, &i));
                continue;
            }
            while (!isOpStackEmpty(&ops) && peekOp(&ops) != '(' &&
                   getPrecedence(peekOp(&ops)) >= getPrecedence(expr[i])) {
                if (expr[i] == '^' && peekOp(&ops) == '^') break;
                double b = popNum(&nums), a = popNum(&nums);
                pushNum(&nums, applyOp(a, b, popOp(&ops)));
            }
            pushOp(&ops, expr[i]);
            i++;
        }
        else i++;
    }
    
    while (!isOpStackEmpty(&ops)) {
        double b = popNum(&nums), a = popNum(&nums);
        pushNum(&nums, applyOp(a, b, popOp(&ops)));
    }
    
    return popNum(&nums);
}

// ==================== SECTION 1: EXPRESSION CALCULATOR ====================

void expressionCalculator() {
    char expr[MAX_EXPR];
    char choice;
    
    do {
        printf("\n=== Expression Calculator ===\n");
        printf("Supported: + - * / ^ () sqrt() log() sin() cos() tan()\n");
        printf("Enter expression: ");
        fgets(expr, MAX_EXPR, stdin);
        expr[strcspn(expr, "\n")] = '\0';
        
        if (strlen(expr) > 0) {
            printf("Result: %lf\n", evaluateExpr(expr));
        }
        
        printf("Continue? (y/n): ");
        scanf(" %c", &choice);
        clearBuffer();
    } while (choice == 'y' || choice == 'Y');
}

// ==================== SECTION 2: PHYSICS EQUATIONS ====================

void solveNewton() {
    double m, a;
    printf("Enter mass (kg): ");
    scanf("%lf", &m);
    printf("Enter acceleration (m/s2): ");
    scanf("%lf", &a);
    clearBuffer();
    printf("Force = %lf N\n", m * a);
}

void solveKineticEnergy() {
    double m, v;
    printf("Enter mass (kg): ");
    scanf("%lf", &m);
    printf("Enter velocity (m/s): ");
    scanf("%lf", &v);
    clearBuffer();
    printf("Kinetic Energy = %lf J\n", 0.5 * m * v * v);
}

void solveOhm() {
    double i, r;
    printf("Enter current (A): ");
    scanf("%lf", &i);
    printf("Enter resistance (Ohm): ");
    scanf("%lf", &r);
    clearBuffer();
    printf("Voltage = %lf V\n", i * r);
}

void solvePotentialEnergy() {
    double m, h;
    printf("Enter mass (kg): ");
    scanf("%lf", &m);
    printf("Enter height (m): ");
    scanf("%lf", &h);
    clearBuffer();
    printf("Potential Energy = %lf J\n", m * GRAVITY * h);
}

void solveVelocity() {
    double d, t;
    printf("Enter distance (m): ");
    scanf("%lf", &d);
    printf("Enter time (s): ");
    scanf("%lf", &t);
    clearBuffer();
    if (t != 0) printf("Velocity = %lf m/s\n", d / t);
    else printf("Error: time cannot be zero\n");
}

void solveMomentum() {
    double m, v;
    printf("Enter mass (kg): ");
    scanf("%lf", &m);
    printf("Enter velocity (m/s): ");
    scanf("%lf", &v);
    clearBuffer();
    printf("Momentum = %lf kg.m/s\n", m * v);
}

void solveWork() {
    double f, d, angle;
    printf("Enter force (N): ");
    scanf("%lf", &f);
    printf("Enter distance (m): ");
    scanf("%lf", &d);
    printf("Enter angle (degrees): ");
    scanf("%lf", &angle);
    clearBuffer();
    printf("Work = %lf J\n", f * d * cos(angle * PI / 180));
}

void solvePower() {
    double w, t;
    printf("Enter work (J): ");
    scanf("%lf", &w);
    printf("Enter time (s): ");
    scanf("%lf", &t);
    clearBuffer();
    if (t != 0) printf("Power = %lf W\n", w / t);
    else printf("Error: time cannot be zero\n");
}

void physicsEquations() {
    int choice;
    char cont;
    
    do {
        printf("\n=== Physics Equations ===\n");
        printf("1) Newton's Second Law    F = m * a\n");
        printf("2) Kinetic Energy         KE = 0.5 * m * v^2\n");
        printf("3) Ohm's Law              V = I * R\n");
        printf("4) Potential Energy       PE = m * g * h\n");
        printf("5) Velocity               v = d / t\n");
        printf("6) Momentum               p = m * v\n");
        printf("7) Work                   W = F * d * cos(angle)\n");
        printf("8) Power                  P = W / t\n");
        printf("9) Back to main menu\n");
        printf("Choice: ");
        scanf("%d", &choice);
        clearBuffer();
        
        switch (choice) {
            case 1: solveNewton(); break;
            case 2: solveKineticEnergy(); break;
            case 3: solveOhm(); break;
            case 4: solvePotentialEnergy(); break;
            case 5: solveVelocity(); break;
            case 6: solveMomentum(); break;
            case 7: solveWork(); break;
            case 8: solvePower(); break;
            case 9: return;
        }
        
        printf("Continue? (y/n): ");
        scanf(" %c", &cont);
        clearBuffer();
    } while (cont == 'y' || cont == 'Y');
}

// ==================== SECTION 3: LINEAR & QUADRATIC EQUATIONS ====================

void solveLinear() {
    double a, b;
    printf("Equation: ax + b = 0\n");
    printf("Enter a: ");
    scanf("%lf", &a);
    printf("Enter b: ");
    scanf("%lf", &b);
    clearBuffer();
    
    if (a == 0) {
        if (b == 0) printf("Infinite solutions\n");
        else printf("No solution\n");
    } else {
        printf("x = %lf\n", -b / a);
    }
}

void solveQuadratic() {
    double a, b, c, disc, x1, x2;
    printf("Equation: ax^2 + bx + c = 0\n");
    printf("Enter a: ");
    scanf("%lf", &a);
    printf("Enter b: ");
    scanf("%lf", &b);
    printf("Enter c: ");
    scanf("%lf", &c);
    clearBuffer();
    
    if (a == 0) {
        if (b != 0) printf("x = %lf\n", -c / b);
        else printf("Invalid equation\n");
        return;
    }
    
    disc = b * b - 4 * a * c;
    printf("Discriminant = %lf\n", disc);
    
    if (disc > 0) {
        x1 = (-b + sqrt(disc)) / (2 * a);
        x2 = (-b - sqrt(disc)) / (2 * a);
        printf("Two real roots: x1 = %lf, x2 = %lf\n", x1, x2);
    } else if (disc == 0) {
        x1 = -b / (2 * a);
        printf("One repeated root: x = %lf\n", x1);
    } else {
        double real = -b / (2 * a);
        double imag = sqrt(-disc) / (2 * a);
        printf("Complex roots: x1 = %lf + %lfi, x2 = %lf - %lfi\n", real, imag, real, imag);
    }
}

void equationsSolver() {
    int choice;
    char cont;
    
    do {
        printf("\n=== Linear & Quadratic Equations ===\n");
        printf("1) Linear equation (ax + b = 0)\n");
        printf("2) Quadratic equation (ax^2 + bx + c = 0)\n");
        printf("3) Back to main menu\n");
        printf("Choice: ");
        scanf("%d", &choice);
        clearBuffer();
        
        switch (choice) {
            case 1: solveLinear(); break;
            case 2: solveQuadratic(); break;
            case 3: return;
        }
        
        printf("Continue? (y/n): ");
        scanf(" %c", &cont);
        clearBuffer();
    } while (cont == 'y' || cont == 'Y');
}

// ==================== SECTION 4: NUMBER SYSTEM CONVERSIONS ====================

void decToBin(int dec) {
    int bin[64], i = 0, j;
    int orig = dec;
    
    if (dec < 0) dec = -dec;
    if (dec == 0) { printf("Binary: 0\n"); return; }
    
    while (dec > 0) {
        bin[i++] = dec % 2;
        dec /= 2;
    }
    
    printf("Decimal: %d\nBinary: ", orig);
    if (orig < 0) printf("-");
    for (j = i - 1; j >= 0; j--) printf("%d", bin[j]);
    printf("\n");
}

int binToDec(char *bin) {
    int dec = 0, len = strlen(bin), i;
    int power = 1;
    
    for (i = len - 1; i >= 0; i--) {
        if (bin[i] != '0' && bin[i] != '1') {
            printf("Error: invalid binary\n");
            return 0;
        }
        dec += (bin[i] - '0') * power;
        power *= 2;
    }
    return dec;
}

void decToOct(int dec) {
    int oct[32], i = 0, j;
    int orig = dec;
    
    if (dec < 0) dec = -dec;
    if (dec == 0) { printf("Octal: 0\n"); return; }
    
    while (dec > 0) {
        oct[i++] = dec % 8;
        dec /= 8;
    }
    
    printf("Decimal: %d\nOctal: ", orig);
    if (orig < 0) printf("-");
    for (j = i - 1; j >= 0; j--) printf("%d", oct[j]);
    printf("\n");
}

void decToHex(int dec) {
    char hex[32];
    int i = 0, j, rem;
    int orig = dec;
    
    if (dec < 0) dec = -dec;
    if (dec == 0) { printf("Hexadecimal: 0\n"); return; }
    
    while (dec > 0) {
        rem = dec % 16;
        hex[i++] = (rem < 10) ? ('0' + rem) : ('A' + rem - 10);
        dec /= 16;
    }
    
    printf("Decimal: %d\nHexadecimal: ", orig);
    if (orig < 0) printf("-");
    printf("0x");
    for (j = i - 1; j >= 0; j--) printf("%c", hex[j]);
    printf("\n");
}

void numberConversions() {
    int choice, num;
    char bin[65], cont;
    
    do {
        printf("\n=== Number System Conversions ===\n");
        printf("1) Decimal to Binary\n");
        printf("2) Binary to Decimal\n");
        printf("3) Decimal to Octal\n");
        printf("4) Decimal to Hexadecimal\n");
        printf("5) Back to main menu\n");
        printf("Choice: ");
        scanf("%d", &choice);
        clearBuffer();
        
        switch (choice) {
            case 1:
                printf("Enter decimal: ");
                scanf("%d", &num);
                clearBuffer();
                decToBin(num);
                break;
            case 2:
                printf("Enter binary: ");
                scanf("%s", bin);
                clearBuffer();
                printf("Decimal: %d\n", binToDec(bin));
                break;
            case 3:
                printf("Enter decimal: ");
                scanf("%d", &num);
                clearBuffer();
                decToOct(num);
                break;
            case 4:
                printf("Enter decimal: ");
                scanf("%d", &num);
                clearBuffer();
                decToHex(num);
                break;
            case 5:
                return;
        }
        
        printf("Continue? (y/n): ");
        scanf(" %c", &cont);
        clearBuffer();
    } while (cont == 'y' || cont == 'Y');
}

// ==================== MAIN FUNCTION ====================

int main() {
    int choice;
    
    do {
        printf("\n=== Scientific Calculator ===\n");
        printf("1) Expression Calculator\n");
        printf("2) Physics Equations\n");
        printf("3) Linear & Quadratic Equations\n");
        printf("4) Number System Conversions\n");
        printf("5) Exit\n");
        printf("Choice: ");
        
        if (scanf("%d", &choice) != 1) {
            clearBuffer();
            printf("Invalid input\n");
            continue;
        }
        clearBuffer();
        
        switch (choice) {
            case 1: expressionCalculator(); break;
            case 2: physicsEquations(); break;
            case 3: equationsSolver(); break;
            case 4: numberConversions(); break;
            case 5: printf("Goodbye!\n"); break;
            default: printf("Invalid choice\n");
        }
    } while (choice != 5);
    
    return 0;
}