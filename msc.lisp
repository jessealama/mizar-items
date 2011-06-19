
(in-package :mizar)

(define-msc2010-classification 00-XX
    :name "00-XX"
    :description "General")

(define-msc2010-classification 01-XX
    :name "01-XX"
    :description "History and biography"
    :see-also nil)

(define-msc2010-classification 03-XX
    :name "03-XX"
    :description "Mathematical logic and foundations"
    :see-also '("See also the classification number -03 in the other sections"))

(define-msc2010-classification 05-XX
    :name "05-XX"
    :description "Combinatorics"
    :see-also '(("Finite fields" 11T)))

(define-msc2010-classification 06-XX
    :name "06-XX"
    :description "Order, lattices, ordered algebraic structures"
    :see-also '(18B35))

(define-msc2010-classification 08-XX
    :name "08-XX"
    :description "General algebraic systems")

(define-msc201-classification 11-XX
    :name "11-XX"
    :description "Number theory")

(define-msc2010-classification 12-XX
    :name "12-XX"
    :description "Field theory and polynomials")

(define-msc-2010-classification 13-XX
    :name "13-XX"
    :description "Commutative algebra rings and algebras")

(define-msc-2010-classification 14-XX
    :name "14-XX"
    :description "Algebraic geometry")

(define-msc-2010-classification 15-XX
    :name "15-XX"
    :description "Linear and multilinear algebra; matrix theory")

(define-msc-2010-classification 16-XX
    :name "16-XX"
    :description "Associative rings and algebras"
    :see-also '(("The commutative case" 13-XX)))

(define-msc-2010-classification 17-XX
    :name "17-XX"
    :description "Nonassociative rings and algebras")

(define-msc-2010-classification 18-XX
    :name "18-XX"
    :description "Category theory; homological algebra"
    :see-also '(("Commutative rings" 13Dxx)
		("Associative rings" 16Exx)
		("Groups" 20Jxx)
		("Topological groups and related structures" 57Txx)
		("Algebraic topology" 55Nxx 55Uxx)))

(define-msc-2010-classification 19-XX
    :name "19-XX"
    :description "$K$-theory"
    :see-also '(16E20 18F25))

(define-msc-2010-classification 20-XX
    :name "20-XX"
    :description "Group theory and generalizations")

(define-msc-2010-classification 22-XX
    :name "22-XX"
    :description "Topological groups, Lie groups"
    :see-also '(("Topological groups" 54H15 57Sxx 58-XX)
		("Abstract harmonic analysis" 43-XX)))

(define-msc-2010-classification 26-XX
    :name "26-XX"
    :description "Real functions"
    :see-also '(54C30))

(define-msc-2010-classification 28-XX
    :name "28-XX"
    :description "Measure and integration"
    :see-also '(("Analysis on manifolds" 58-XX)))

(define-msc-2010-classification 30-XX
    :name "30-XX"
    :description "Functions of a complex variable"
    :see-also '(("Analysis on manifolds" 58-XX)))

(define-msc-2010-classification 31-XX
    :name "31-XX"
    :description "Potential theory"
    :see-also '(("Probabilistic potential theory" 60J45})))

(define-msc-2010-classification 32-XX
    :name "32-XX"
    :description "Several complex variables and analytic spaces"
    :see-also '("For infinite-dimensional holomorphy" 46G20 58B12))

(define-msc-2010-classification 33-XX
    :name "33-XX"
    :description "Special functions"
    :see-also '(("Properties of functions as functions" 33-XX)
		("Orthogonal functions" 42Cxx)
		("Aspects of combinatorics" 05Axx)
		("Number-theoretic aspects" 11-XX)
		("Representation theory") 22Exx))

(define-msc-2010-classification 34-XX
    :name "34-XX"
    :description "Ordinary differential equations")

(define-msc-2010-classification 35-XX
    :name "35-XX"
    :description "Partial differential equations")

(define-msc-2010-classification 37-XX
    :name "37-XX"
    :description "Dynamical systems and ergodic theory"
    :see-also '(26A18 28Dxx 34Cxx 34Dxx 35Bxx 46Lxx 58Jxx 70-XX))

(define-msc-2010-classification 39-XX
    :name "39-XX"
    :description "Difference and functional equations")

(define-msc-2010-classification 40-XX
    :name "40-XX"
    :description "Sequences, series, summability")

(define-msc-2010-classification 41-XX
    :name "41-XX"
    :description "Approximations and expansions"
    :see-also '(("Approximation theory in the complex domain" 30E05 30E10)
		("Trigonometric approximation and interpolation" 42A10 42A15)
		("Numerical approximation" 65Dxx)))

(define-msc-2010-classification 42-XX
    :name "42-XX"
    :description "Harmonic analysis on Euclidean spaces Fourier analysis")

(define-msc-2010-classification 43-XX
    :name "43-XX"
    :description "Abstract harmonic analysis"
    :see-also '("Other analysis on topological and Lie groups" 22Exx))

(define-msc-2010-classification 44-XX
    :name "44-XX"
    :description "Integral transforms, operational calculus"
    :see-also '(("For fractional derivatives and integrals" 26A33)
		("Fourier transforms" 42A38 42B10)
		("Integral transforms in distribution spaces" 46F12)
		("Numerical methods" 65R10)))

(define-msc-2010-classification 45-XX
    :name "45-XX"
    :description "Integral equations")

(define-msc-2010-classification 46-XX
    :name "46-XX"
    :description "Functional analysis"
    :see-also (("Manifolds modeled on topological linear spaces" 57Nxx 58Bxx)))

(define-msc-2010-classification 47-XX
    :name "47-XX"
    :description "Operator theory")

(define-msc-2010-classification 49-XX
    :name "49-XX"
    :description "Calculus of variations and optimal control; optimization"
    :see-also '(34H05 34K35 65Kxx 90Cxx 93-XX))

(define-msc-2010-classification 51-XX
    :name "51-XX"
    :description "Geometry"
    :see-also '(("Algebraic geometry" 14-XX)))

(define-msc-2010-classification 52-XX
    :name "52-XX"
    :description "Convex and discrete geometry")

(define-msc-2010-classification 53-XX
    :name "53-XX"
    :description "Differential geometry"
    :see-also '(("Differential topology" 57Rxx)
		("Foundational questions of differentiable manifolds" 58Axx)))

(define-msc-2010-classification 54-XX
    :name "54-XX"
    :description "General topology"
    :see-also (("The topology of manifolds of all dimensions" 57Nxx)))

(define-msc-2010-classification 55-XX
    :name "55-XX"
    :description "Algebraic topology")

(define-msc-2010-classification 57-XX
    :name "57-XX"
    :description "Manifolds and cell complexes"
    :see-also '(("Complex manifolds" 32Qxx)))

(define-msc-2010-classification 58-XX
    :name "58-XX"
    :description "Global analysis, analysis on manifolds"
    :see-also '(("Geometric integration theory" 49Q15)
		32Cxx 32Fxx 32Wxx 46-XX 47Hxx 53Cxx))

(define-msc-2010-classification 60-XX
    :name "60-XX"
    :description "Probability theory and stochastic processes"
    :see-also '(("Applications" 11Kxx 62-XX 90-XX 91-XX 92-XX 93-XX 94-XX)))

(define-msc-2010-classification 62-XX
    :name "62-XX"
    :description "Statistics")

(define-msc-2010-classification 65-XX
    :name "65-XX"
    :description "Numerical analysis")

;; special case: refers to many subareas at once
(define-msc-2010-classification 68-XX
    :name "68-XX"
    :description "Computer science"
    :see-also '("Papers involving machine computations and programs in a specific mathematical area, see Section -04 in that area"))

(define-msc-2010-classification 70-XX
    :name "70-XX"
    :description "Mechanics of particles and systems"
    :see-also '(("Relativistic mechanics" 83A05 83C10)
		("Statistical mechanics" 82-XX)))

(define-msc-2010-classification 74-XX
    :name "74-XX"
    :description "Mechanics of deformable solids")

(define-msc-2010-classification 76-XX
    :name "76-XX"
    :description "Fluid mechanics"
    :see-also '(("General continuum mechanics" 74Axx 74-XX)))

(define-msc-2010-classification 78-XX
    :name "78-XX"
    :description "Optics, electromagnetic theory"
    :see-also '(("Quantum optics" 81V80)))

(define-msc-2010-classification 80-XX
    :name "80-XX"
    :description "Classical thermodynamics, heat transfer"
    :see-also '(("Thermodynamics of solids" 74A15)))

(define-msc-2010-classification 81-XX
    :name "81-XX"
    :description "Quantum theory")

(define-msc-2010-classification 82-XX
    :name "82-XX"
    :description "Statistical mechanics, structure of matter")

(define-msc-2010-classification 83-XX
    :name "83-XX"
    :description "Relativity and gravitational theory")

(define-msc-2010-classification 85-XX
    :name "85-XX"
    :description "Astronomy and astrophysics"
    :see-also '(("Celestial mechanics" 70F15)))

(define-msc-2010-classification 86-XX
    :name "86-XX"
    :description "Geophysics"
    :see-also '(76U05 76V05))

(define-msc-2010-classification 90-XX
    :name "90-XX"
    :description "Operations research, mathematical programming")

(define-msc-2010-classification 91-XX
    :name "91-XX"
    :description "Game theory, economics, social and behavioral sciences")

(define-msc-2010-classification 92-XX
    :name "92-XX"
    :description "Biology and other natural sciences")

(define-msc-2010-classification 93-XX
    :name "93-XX"
    :description "Systems theory; control"
    :see-also '(("Optimal control" 49-XX)))

(define-msc-2010-classification 94-XX
    :name "94-XX"
    :description "Information and communication, circuits")

(define-msc-2010-classification 97-XX
    :name "97-XX"
    :description "Mathematics education")
