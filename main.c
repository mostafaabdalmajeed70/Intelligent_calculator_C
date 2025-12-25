#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

#define MAX_EXPR 256
#define MAX_STACK 100
#define PI 3.14159265358979
#define GRAVITY 9.80665
#define COULOMB_K 8.99e9
#define GRAVITY_CONST 6.674e-11
#define PLANCK 6.626e-34

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

void solveTotalEnergy() {
    double m, v, h;
    printf("Enter mass (kg): ");
    scanf("%lf", &m);
    printf("Enter velocity (m/s): ");
    scanf("%lf", &v);
    printf("Enter height (m): ");
    scanf("%lf", &h);
    clearBuffer();
    
    double ke = 0.5 * m * v * v;
    double pe = m * GRAVITY * h;
    double total = ke + pe;
    
    printf("Kinetic Energy = %lf J\n", ke);
    printf("Potential Energy = %lf J\n", pe);
    printf("Total Mechanical Energy = %lf J\n", total);
}

void solveFinalVelocity() {
    double v_i, a, t;
    printf("Enter initial velocity (m/s): ");
    scanf("%lf", &v_i);
    printf("Enter acceleration (m/s²): ");
    scanf("%lf", &a);
    printf("Enter time (s): ");
    scanf("%lf", &t);
    clearBuffer();
    
    double v_f = v_i + a * t;
    printf("Final Velocity = %lf m/s\n", v_f);
}

void solveDisplacement() {
    double v_i, a, t;
    printf("Enter initial velocity (m/s): ");
    scanf("%lf", &v_i);
    printf("Enter acceleration (m/s²): ");
    scanf("%lf", &a);
    printf("Enter time (s): ");
    scanf("%lf", &t);
    clearBuffer();
    
    double s = v_i * t + 0.5 * a * t * t;
    printf("Displacement = %lf m\n", s);
}

void solveCoulombForce() {
    double q1, q2, r;
    printf("Enter charge 1 (C): ");
    scanf("%lf", &q1);
    printf("Enter charge 2 (C): ");
    scanf("%lf", &q2);
    printf("Enter distance (m): ");
    scanf("%lf", &r);
    clearBuffer();
    
    if (r != 0) {
        double f = COULOMB_K * (q1 * q2) / (r * r);
        printf("Coulomb Force = %lf N\n", f);
    } else {
        printf("Error: distance cannot be zero\n");
    }
}

void solveResistance() {
    double rho, length, area;
    printf("Enter resistivity (Ohm·m): ");
    scanf("%lf", &rho);
    printf("Enter length (m): ");
    scanf("%lf", &length);
    printf("Enter cross-sectional area (m²): ");
    scanf("%lf", &area);
    clearBuffer();
    
    if (area != 0) {
        double r = rho * length / area;
        printf("Resistance = %lf Ohm\n", r);
    } else {
        printf("Error: area cannot be zero\n");
    }
}

void solveElectricalEnergy() {
    double v, i, t;
    printf("Enter voltage (V): ");
    scanf("%lf", &v);
    printf("Enter current (A): ");
    scanf("%lf", &i);
    printf("Enter time (s): ");
    scanf("%lf", &t);
    clearBuffer();
    
    double energy = v * i * t;
    printf("Electrical Energy = %lf J\n", energy);
}

void solveCentripetal() {
    double v, r;
    printf("Enter velocity (m/s): ");
    scanf("%lf", &v);
    printf("Enter radius (m): ");
    scanf("%lf", &r);
    clearBuffer();
    
    if (r != 0) {
        double a_c = (v * v) / r;
        printf("Centripetal Acceleration = %lf m/s²\n", a_c);
    } else {
        printf("Error: radius cannot be zero\n");
    }
}

void solveCentripetalForce() {
    double m, v, r;
    printf("Enter mass (kg): ");
    scanf("%lf", &m);
    printf("Enter velocity (m/s): ");
    scanf("%lf", &v);
    printf("Enter radius (m): ");
    scanf("%lf", &r);
    clearBuffer();
    
    if (r != 0) {
        double f_c = m * (v * v) / r;
        printf("Centripetal Force = %lf N\n", f_c);
    } else {
        printf("Error: radius cannot be zero\n");
    }
}

void solveGravitationalForce() {
    double m1, m2, r;
    printf("Enter mass 1 (kg): ");
    scanf("%lf", &m1);
    printf("Enter mass 2 (kg): ");
    scanf("%lf", &m2);
    printf("Enter distance (m): ");
    scanf("%lf", &r);
    clearBuffer();
    
    if (r != 0) {
        double f = GRAVITY_CONST * (m1 * m2) / (r * r);
        printf("Gravitational Force = %lf N\n", f);
    } else {
        printf("Error: distance cannot be zero\n");
    }
}

void solveWaveVelocity() {
    double f, lambda;
    printf("Enter frequency (Hz): ");
    scanf("%lf", &f);
    printf("Enter wavelength (m): ");
    scanf("%lf", &lambda);
    clearBuffer();
    
    double v = f * lambda;
    printf("Wave Velocity = %lf m/s\n", v);
}

void solvePhotonEnergy() {
    double f;
    printf("Enter frequency (Hz): ");
    scanf("%lf", &f);
    clearBuffer();
    
    double energy = PLANCK * f;
    printf("Photon Energy = %lf J\n", energy);
}

void solvePressure() {
    double f, a;
    printf("Enter force (N): ");
    scanf("%lf", &f);
    printf("Enter area (m²): ");
    scanf("%lf", &a);
    clearBuffer();
    
    if (a != 0) {
        double p = f / a;
        printf("Pressure = %lf Pa\n", p);
    } else {
        printf("Error: area cannot be zero\n");
    }
}

void solveDynamicPower() {
    double f, v, angle;
    printf("Enter force (N): ");
    scanf("%lf", &f);
    printf("Enter velocity (m/s): ");
    scanf("%lf", &v);
    printf("Enter angle (degrees): ");
    scanf("%lf", &angle);
    clearBuffer();
    
    double p = f * v * cos(angle * PI / 180);
    printf("Power = %lf W\n", p);
}

void physicsEquations() {
    int choice;
    char cont;
    
    do {
        printf("\n=== Physics Equations ===\n");
        printf("1) Newton's Second Law       (F = m*a)\n");
        printf("2) Kinetic Energy            (KE = 0.5*m*v²)\n");
        printf("3) Ohm's Law                 (V = I*R)\n");
        printf("4) Potential Energy          (PE = m*g*h)\n");
        printf("5) Velocity                  (v = d/t)\n");
        printf("6) Momentum                  (p = m*v)\n");
        printf("7) Work                      (W = F*d*cos(θ))\n");
        printf("8) Power                     (P = W/t)\n");
        printf("9) Total Mechanical Energy   (E = KE + PE)\n");
        printf("10) Final Velocity           (v_f = v_i + a*t)\n");
        printf("11) Displacement             (s = v_i*t + 0.5*a*t²)\n");
        printf("12) Coulomb's Law            (F = k*q1*q2/r²)\n");
        printf("13) Electrical Resistance    (R = ρ*L/A)\n");
        printf("14) Electrical Energy        (E = V*I*t)\n");
        printf("15) Centripetal Acceleration (a = v²/r)\n");
        printf("16) Centripetal Force        (F = m*v²/r)\n");
        printf("17) Gravitational Force      (F = G*m1*m2/r²)\n");
        printf("18) Wave Velocity            (v = f*λ)\n");
        printf("19) Photon Energy            (E = h*f)\n");
        printf("20) Pressure                 (P = F/A)\n");
        printf("21) Dynamic Power            (P = F*v*cos(θ))\n");
        printf("22) Back to main menu\n");
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
            case 9: solveTotalEnergy(); break;
            case 10: solveFinalVelocity(); break;
            case 11: solveDisplacement(); break;
            case 12: solveCoulombForce(); break;
            case 13: solveResistance(); break;
            case 14: solveElectricalEnergy(); break;
            case 15: solveCentripetal(); break;
            case 16: solveCentripetalForce(); break;
            case 17: solveGravitationalForce(); break;
            case 18: solveWaveVelocity(); break;
            case 19: solvePhotonEnergy(); break;
            case 20: solvePressure(); break;
            case 21: solveDynamicPower(); break;
            case 22: return;
        }
        
        printf("Continue? (y/n): ");
        scanf(" %c", &cont);
        clearBuffer();
    } while (cont == 'y' || cont == 'Y');
}

// ==================== SECTION 3: SMART EQUATION SOLVER ====================

double extractCoefficient(char *term, int len) {
    char coef[32];
    int i = 0, j = 0;
    int sign = 1;
    
    while (i < len && isspace(term[i])) i++;
    
    if (term[i] == '-') { sign = -1; i++; }
    else if (term[i] == '+') { i++; }
    
    while (i < len && isspace(term[i])) i++;
    
    while (i < len && (isdigit(term[i]) || term[i] == '.')) {
        coef[j++] = term[i++];
    }
    coef[j] = '\0';
    
    if (j == 0) return sign * 1.0;
    return sign * atof(coef);
}

void parseEquation(char *equation, double *a, double *b, double *c, double *d, int *degree) {
    char left[MAX_EXPR], right[MAX_EXPR];
    char *eq_sign;
    int i, len;
    double rightVal = 0;
    
    *a = 0; *b = 0; *c = 0; *d = 0; *degree = 0;
    
    eq_sign = strchr(equation, '=');
    if (eq_sign) {
        len = eq_sign - equation;
        strncpy(left, equation, len);
        left[len] = '\0';
        strcpy(right, eq_sign + 1);
        rightVal = atof(right);
    } else {
        strcpy(left, equation);
    }
    
    len = strlen(left);
    i = 0;
    
    while (i < len) {
        while (i < len && isspace(left[i])) i++;
        if (i >= len) break;
        
        int termStart = i;
        int sign = 1;
        
        if (left[i] == '+') { sign = 1; i++; }
        else if (left[i] == '-') { sign = -1; i++; }
        
        while (i < len && isspace(left[i])) i++;
        
        double coef = 0;
        int hasCoef = 0;
        
        while (i < len && (isdigit(left[i]) || left[i] == '.')) {
            if (!hasCoef) coef = 0;
            hasCoef = 1;
            if (left[i] == '.') {
                i++;
                double decimal = 0.1;
                while (i < len && isdigit(left[i])) {
                    coef += (left[i] - '0') * decimal;
                    decimal *= 0.1;
                    i++;
                }
            } else {
                coef = coef * 10 + (left[i] - '0');
                i++;
            }
        }
        
        if (!hasCoef) coef = 1;
        coef *= sign;
        
        while (i < len && isspace(left[i])) i++;
        
        if (i < len && (left[i] == 'x' || left[i] == 'X')) {
            i++;
            while (i < len && isspace(left[i])) i++;
            
            if (i < len && left[i] == '^') {
                i++;
                while (i < len && isspace(left[i])) i++;
                
                int exp = 0;
                while (i < len && isdigit(left[i])) {
                    exp = exp * 10 + (left[i] - '0');
                    i++;
                }
                
                if (exp == 3) { *a += coef; if (*degree < 3) *degree = 3; }
                else if (exp == 2) { *b += coef; if (*degree < 2) *degree = 2; }
                else if (exp == 1) { *c += coef; if (*degree < 1) *degree = 1; }
            } else {
                *c += coef;
                if (*degree < 1) *degree = 1;
            }
        } else {
            *d += coef;
        }
    }
    
    *d -= rightVal;
}

void solveCubicEquation(double a, double b, double c, double d) {
    double p, q, disc;
    double b_a = b / a, c_a = c / a, d_a = d / a;
    
    p = c_a - (b_a * b_a) / 3.0;
    q = d_a - (b_a * c_a) / 3.0 + (2.0 * b_a * b_a * b_a) / 27.0;
    
    disc = (q * q) / 4.0 + (p * p * p) / 27.0;
    
    printf("Discriminant = %lf\n", disc);
    
    if (disc > 0) {
        double u = cbrt(-q/2.0 + sqrt(disc));
        double v = cbrt(-q/2.0 - sqrt(disc));
        double x1 = u + v - b_a/3.0;
        printf("One real root: x = %lf\n", x1);
    } else if (disc == 0) {
        double u = cbrt(-q/2.0);
        double x1 = 2*u - b_a/3.0;
        double x2 = -u - b_a/3.0;
        printf("Three real roots (two equal): x1 = %lf, x2 = x3 = %lf\n", x1, x2);
    } else {
        double r = sqrt(-(p*p*p)/27.0);
        double theta = acos(-q/(2.0*r));
        double m = 2.0 * cbrt(r);
        
        double x1 = m * cos(theta/3.0) - b_a/3.0;
        double x2 = m * cos((theta + 2.0*PI)/3.0) - b_a/3.0;
        double x3 = m * cos((theta + 4.0*PI)/3.0) - b_a/3.0;
        printf("Three real roots: x1 = %lf, x2 = %lf, x3 = %lf\n", x1, x2, x3);
    }
}

void solveQuadraticEquation(double a, double b, double c) {
    double disc = b*b - 4*a*c;
    printf("Discriminant = %lf\n", disc);
    
    if (disc > 0) {
        double x1 = (-b + sqrt(disc)) / (2*a);
        double x2 = (-b - sqrt(disc)) / (2*a);
        printf("Two real roots: x1 = %lf, x2 = %lf\n", x1, x2);
    } else if (disc == 0) {
        printf("One repeated root: x = %lf\n", -b/(2*a));
    } else {
        double real = -b / (2*a);
        double imag = sqrt(-disc) / (2*a);
        printf("Complex roots: x1 = %lf + %lfi, x2 = %lf - %lfi\n", real, imag, real, imag);
    }
}

void solveLinearEquation(double a, double b) {
    if (a == 0) {
        if (b == 0) printf("Infinite solutions (identity)\n");
        else printf("No solution (contradiction)\n");
    } else {
        printf("x = %lf\n", -b/a);
    }
}

void smartEquationSolver() {
    char equation[MAX_EXPR];
    char cont;
    double a, b, c, d;
    int degree;
    
    do {
        printf("\n=== Smart Equation Solver ===\n");
        printf("Enter equation (examples: 2x+3=0, x^2-4x+3=0, x^3-6x^2+11x-6=0)\n");
        printf("Equation: ");
        fgets(equation, MAX_EXPR, stdin);
        equation[strcspn(equation, "\n")] = '\0';
        
        if (strlen(equation) == 0) continue;
        
        parseEquation(equation, &a, &b, &c, &d, &degree);
        
        printf("\nParsed equation:\n");
        if (degree == 3) {
            printf("Cubic: %.2lfx^3 + %.2lfx^2 + %.2lfx + %.2lf = 0\n", a, b, c, d);
            printf("Type: Cubic Equation (Degree 3)\n\n");
            solveCubicEquation(a, b, c, d);
        } else if (degree == 2) {
            printf("Quadratic: %.2lfx^2 + %.2lfx + %.2lf = 0\n", b, c, d);
            printf("Type: Quadratic Equation (Degree 2)\n\n");
            solveQuadraticEquation(b, c, d);
        } else if (degree == 1) {
            printf("Linear: %.2lfx + %.2lf = 0\n", c, d);
            printf("Type: Linear Equation (Degree 1)\n\n");
            solveLinearEquation(c, d);
        } else {
            if (d == 0) printf("Identity: 0 = 0 (Infinite solutions)\n");
            else printf("Contradiction: %.2lf = 0 (No solution)\n", d);
        }
        
        printf("\nContinue? (y/n): ");
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
            case 3: smartEquationSolver(); break;
            case 4: numberConversions(); break;
            case 5: printf("Goodbye!\n"); break;
            default: printf("Invalid choice\n");
        }
    } while (choice != 5);
    
    return 0;
}