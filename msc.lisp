
(in-package :mizar)

(define-msc2010-classification 00-XX
    :name "00-XX"
    :description "General"
    :contains '(00-01 00-02 00Axx 00Bxx))

(define-msc2010-classification 01-XX
    :name "01-XX"
    :description "History and biography"
    :see-also nil)

;; should expand this when we know the full list of "-03 in the other sections" 
(define-msc2010-classification 03-XX
    :name "03-XX"
    :description "Mathematical logic and foundations"
    :see-also '("See also the classification number -03 in the other sections"))

(define-msc2010-classification 05-XX
    :name "05-XX"
    :description "Combinatorics"
    :see-also '(("Finite fields" 11T))
    :contains (05-00 05-01 05-02 05-03 05-04 05-06 05Axx 05Bxx 05Cxx 05Dxx 05Exx))

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
    :description "Commutative algebra")

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

;; 00-XX

(define-msc-2010-classification 00-01
    :name "00-01"
    :description "Instructional exposition (textbooks, tutorial papers, etc.)"
    :parent 00-XX)

(define-msc-2010-classification 00-02
    :name "00-02"
    :description "Research exposition (monographs, survey articles)"
    :parent 00-XX)

(define-msc-2010-classification 00Axx
    :name "00Axx"
    :description "General and miscellaneous specific topics"
    :contains '(00A05 00A06 00A07 00A08 00A09 00A15 00A17 00A20 00A22 00A30 00A35 00A65 00A66 00A67 00A69 00A71 00A72 00A73 00A79 00A99)
    :parent 00-XX)

(define-msc-2010-classification 00Bxx
    :name "00Bxx"
    :description "Conference proceedings and collections of papers"
    :contains '(00B05 00B10 00B15 00B20 00B25 00B30 00B50 00B55 00B60 00B99)
    :parent 00-XX)

;; OOAxx

(define-msc-2010-classification 00A05
    :name "00A05"
    :description "General mathematics"
    :parent 00Axx)

(define-msc-2010-classification 00A06
    :name "00A06"
    :description "Mathematics for nonmathematicians (engineering, social sciences, etc.)"
    :parent 00Axx)

(define-msc-2010-classification 00A07
    :name "00A07"
    :description "Problem books"
    :parent 00Axx)

(define-msc-2010-classification 00A08
    :name "00A08"
    :description "Recreational mathematics"
    :see-also '(97A20)
    :parent 00Axx)

(define-msc-2010-classification 00A09
    :name "00A09"
    :description "Popularization of mathematics"
    :parent 00Axx)

(define-msc-2010-classification 00A15
    :name "00A15"
    :description "Bibliographies"
    :parent 00Axx)

(define-msc-2010-classification 00A17
    :name "00A17"
    :description "External book reviews"
    :parent 00Axx)

(define-msc-2010-classification 00A20
    :name "00A20"
    :description "Dictionaries and other general reference works"
    :parent 00Axx)

(define-msc-2010-classification 00A22
    :name "00A22"
    :description "Formularies"
    :parent 00Axx)

(define-msc-2010-classification 00A30
    :name "00A30"
    :description "Philosophy of mathematics"
    :see-also '(03A05)
    :parent 00Axx)

(define-msc-2010-classification 00A35
    :name "00A35"
    :description "Methodology of mathematics, didactics"
    :see-also '(97Cxx 97Dxx)
    :parent 00Axx)

(define-msc-2010-classification 00A65
    :name "00A65"
    :description "Mathematics and music"
    :parent 00Axx)

(define-msc-2010-classification 00A66
    :name "00A66"
    :description "Mathematics and visual arts, visualization"
    :parent 00Axx)

(define-msc-2010-classification 00A67
    :name "00A67"
    :description "Mathematics and architecture"
    :parent 00Axx)

(define-msc-2010-classification 00A69
    :name "00A69"
    :description "General applied mathematics"
    :see-also '(("Physics" 00A79 70-XX 74-XX 76-XX 78-XX 80-XX 81-XX 82-XX 83-XX 85-XX 86-XX))
    :parent 00Axx)

(define-msc-2010-classification 00A71
    :name "00A71"
    :description "Theory of mathematical modeling"
    :parent 00Axx)

(define-msc-2010-classification 00A72
    :name "00A72"
    :description "General methods of simulation"
    :parent 00Axx)

(define-msc-2010-classification 00A73
    :name "00A73"
    :description "Dimensional analysis"
    :parent 00Axx)

(define-msc-2010-classification 00A79
    :name "00A79"
    :description "Physics"
    :see-also '((70-XX 74-XX 76-XX 78-XX 80-XX 81-XX 82-XX 83-XX 85-XX 86-XX))
    :parent 00Axx)

(define-msc-2010-classification 00A99
    :name "00A99"
    :description "Miscellaneous topics"
    :parent 00Axx)

;; 00Bxx

(define-msc-2010-classification 00B05
    :name "00B05"
    :description "Collections of abstracts of lectures"
    :parent 00Bxx)

(define-msc-2010-classification 00B10
    :name "00B10"
    :description "Collections of articles of general interest"
    :parent 00Bxx)

(define-msc-2010-classification 00B15
    :name "00B15"
    :description "Collections of articles of miscellaneous specific content"
    :parent 00Bxx)

(define-msc-2010-classification 00B20
    :name "00B20"
    :description "Proceedings of conferences of general interest"
    :parent 00Bxx)

(define-msc-2010-classification 00B25
    :name "00B25"
    :description "Proceedings of conferences of miscellaneous specific interest"
    :parent 00Bxx)

(define-msc-2010-classification 00B30
    :name "00B30"
    :description "Festschriften"
    :parent 00Bxx)

(define-msc-2010-classification 00B50
    :name "00B50"
    :description "Volumes of selected translations"
    :parent 00Bxx)

(define-msc-2010-classification 00B55
    :name "00B55"
    :description "Miscellaneous volumes of translations"
    :parent 00Bxx)

(define-msc-2010-classification 00B60
    :name "00B60"
    :description "Collections of reprinted articles"
    :see-also '(01A75)
    :parent 00Bxx)

(define-msc-2010-classification 00B99
    :name "00B99"
    :description "None of the above, but in this section"
    :parent 00Bxx)

;; 01-XX

(define-msc-2010-classification 01-00
    :name "01-00"
    :description "General reference works (handbooks, dictionaries, bibliographies, etc.)"
    :parent 01-XX)

(define-msc-2010-classification 01-01
    :name "01-01"
    :description "Instructional exposition (textbooks, tutorial papers, etc.)"
    :parent 01-XX)

(define-msc-2010-classification 01-02
    :name "01-02"
    :description "Research exposition (monographs, survey articles)"
    :parent 01-XX)

(define-msc-2010-classification 01-06
    :name "01-06"
    :description "Proceedings, conferences, collections, etc."
    :parent 01-XX)

(define-msc-2010-classification 01-08
    :name "01-08"
    :description "Computational methods"
    :parent 01-XX)

(define-msc-2010-classification 01Axx
    :name "01Axx"
    :description "History of mathematics and mathematicians"
    :parent 01-XX)

;; 01Axx

(define-msc-2010-classification 01A05
    :name "01A05"
    :description "General histories, source books"
    :parent 01Axx)

(define-msc-2010-classification 01A07
    :name "01A07"
    :description "Ethnomathematics, general"
    :parent 01Axx)

(define-msc-2010-classification 01A10
    :name "01A10"
    :description "Paleolithic, Neolithic"
    :parent 01Axx)

(define-msc-2010-classification 01A12
    :name "01A12"
    :description "Indigenous cultures of the Americas"
    :parent 01Axx)

(define-msc-2010-classification 01A13
    :name "01A13"
    :description "Other indigenous cultures (non-European)"
    :parent 01Axx)

(define-msc-2010-classification 01A15
    :name "01A15"
    :description "Indigenous European cultures (pre-Greek, etc.)"
    :parent 01Axx)

(define-msc-2010-classification 01A16
    :name "01A16"
    :description "Egyptian"
    :parent 01Axx)

(define-msc-2010-classification 01A17
    :name "01A17"
    :description "Babylonian"
    :parent 01Axx)

(define-msc-2010-classification 01A20
    :name "01A20"
    :description "Greek, Roman"
    :parent 01Axx)

(define-msc-2010-classification 01A25
    :name "01A25"
    :description "China"
    :parent 01Axx)

(define-msc-2010-classification 01A27
    :name "01A27"
    :description "Japan"
    :parent 01Axx)

(define-msc-2010-classification 01A29
    :name "01A29"
    :description "Southeast Asia"
    :parent 01Axx)

(define-msc-2010-classification 01A30
    :name "01A30"
    :description "Islam (Medieval)"
    :parent 01Axx)

(define-msc-2010-classification 01A32
    :name "01A32"
    :description "India"
    :parent 01Axx)

(define-msc-2010-classification 01A35
    :name "01A35"
    :description "Medieval"
    :parent 01Axx)

(define-msc-2010-classification 01A40
    :name "01A40"
    :description "15th and 16th centuries, Renaissance"
    :parent 01Axx)

(define-msc-2010-classification 01A45
    :name "01A45"
    :description "17th century"
    :parent 01Axx)

(define-msc-2010-classification 01A50
    :name "01A50"
    :description "18th century"
    :parent 01Axx)

(define-msc-2010-classification 01A55
    :name "01A55"
    :description "19th century"
    :parent 01Axx)

(define-msc-2010-classification 01A60
    :name "01A60"
    :description "20th century"
    :parent 01Axx)

(define-msc-2010-classification 01A61
    :name "01A61"
    :description "Twenty-first century"
    :parent 01Axx)

(define-msc-2010-classification 01A65
    :name "01A65"
    :description "Contemporary"
    :parent 01Axx)

(define-msc-2010-classification 01A67
    :name "01A67"
    :description "Future prospectives"
    :parent 01Axx)

(define-msc-2010-classification 01A70
    :name "01A70"
    :description "Biographies, obituaries, personalia, bibliographies"
    :parent 01Axx)

(define-msc-2010-classification 01A72
    :name "01A72"
    :description "Schools of mathematics"
    :parent 01Axx)

(define-msc-2010-classification 01A73
    :name "01A73"
    :description "Universities"
    :parent 01Axx)

(define-msc-2010-classification 01A74
    :name "01A74"
    :description "Other institutions and academies"
    :parent 01Axx)

(define-msc-2010-classification 01A75
    :name "01A75"
    :description "Collected or selected works; reprintings or translations of classics"
    :see-also '(00B60)
    :parent 01Axx)

(define-msc-2010-classification 01A80
    :name "01A80"
    :description "Sociology (and profession) of mathematics"
    :parent 01Axx)

(define-msc-2010-classification 01A85
    :name "01A85"
    :description "Historiography"
    :parent 01Axx)

(define-msc-2010-classification 01A90
    :name "01A90"
    :description "Bibliographic studies"
    :parent 01Axx)

(define-msc-2010-classification 01A99
    :name "01A99"
    :description "Miscellaneous topics"
    :parent 01Axx)

;; 03-XX

(define-msc-2010-classification 03-00
    :name "03-00"
    :description "General reference works (handbooks, dictionaries, bibliographies, etc.)"
    :parent 03-XX)

(define-msc-2010-classification 03-01
    :name "03-01"
    :description "Instructional exposition (textbooks, tutorial papers, etc.)"
    :parent 03-XX)

(define-msc-2010-classification 03-02
    :name "03-02"
    :description "Research exposition (monographs, survey articles)"
    :parent 03-XX)

(define-msc-2010-classification 03-03
    :name "03-03"
    :description "Historical"
    :see-also '(01-XX)
    :parent 03-XX)

(define-msc-2010-classification 03-04
    :name "03-04"
    :description "Explicit machine computation and programs (not the theory of computation or programming)"
    :parent 03-XX)

(define-msc-2010-classification 03-06
    :name "03-06"
    :description "Proceedings, conferences, collections, etc."
    :parent 03-XX)

(define-msc-2010-classification 03Axx
    :name "03Axx"
    :description "Philosophical aspects of logic and foundations"
    :parent 03-XX
    :contains (03A05 03A10 03A99))

(define-msc-2010-classification 03Bxx
    :name "03Bxx"
    :description "General logic"
    :parent 03-XX
    :contains (03B05 03B10 03B15 03B20 03B22 03B25 03B30 03B35 03B40 03B42 03B44 03B45 03B47 03B48 03B50 03B52 03B53 03B55 03B60 03B62 03B65 03B70 03B80 03B99))

(define-msc-2010-classification 03Cxx
    :name "03Cxx"
    :description "Model theory"
    :parent 03-XX
    :contains (03C05 03C07 03C10 03C13 03C15 03C20 03C25 03C30 03C35 03C40 03C45 03C48 03C50 03C52 03C55 03C57 03C60 03C62 03C64 03C65 03C68 03C70 03C75 03C80 03C85 03C90 03C95 03C98 03C99))

(define-msc-2010-classification 03Dxx
    :name "03Dxx"
    :description "Computability and recursion theory"
    :parent 03-XX
    :contains (03D03 03D05 03D10 03D15 03D20 03D25 03D28 03D30 03D32 03D35 03D40 03D45 03D50 03D55 03D60 03D65 03D70 03D75 03D78 03D80 03D99))

(define-msc-2010-classification 03Exx
    :name "03Exx"
    :description "Set theory"
    :parent 03-XX
    :contains (03E02 03E04 03E05 03E10 03E15 03E17 03E20 03E25 03E30 03E35 03E40 03E45 03E47 03E50 03E55 03E57 03E60 03E65 03E70 03E72 03E75 03E99))

(define-msc-2010-classification 03Fxx
    :name "03Fxx"
    :description "Proof theory and constructive mathematics"
    :parent 03-XX
    :contains (03F03 03F05 03F07 03F10 03F15 03F20 03F25 03F30 03F35 03F40 03F45 03F50 03F52 03F55 03F60 03F65 03F99))

(define-msc-2010-classification 03Gxx
    :name "03Gxx"
    :description "Algebraic logic"
    :parent 03-XX
    :contains (03G05 03G10 03G12 03G15 03G20 03G25 03G27 03G30 03G99))

(define-msc-2010-classification 03Hxx
    :name "03Hxx"
    :description "Nonstandard models"
    :see-also '(03C62)
    :parent 03-XX
    :contains (03H05 03H10 03H15 03H99))

;; 03Axx

(define-msc-2010-classification 03A05
    :name "03A05"
    :description "Philosophical and critical"
    :see-also '(("Philosophy of mathematics" 00A30))
    :parent 03Axx)

(define-msc-2010-classification 03A10
    :name "03A10"
    :description "Logic in the philosophy of science"
    :parent 03Axx)

(define-msc-2010-classification 03A99
    :name "03A99"
    :description "None of the above, but in this section"
    :parent 03Axx)

;; 03Bxx

(define-msc-2010-classification 03B05
    :name "03B05"
    :description "Classical propositional logic"
    :parent 03Bxx)

(define-msc-2010-classification 03B10
    :name "03B10"
    :description "Classical first-order logic"
    :parent 03Bxx)

(define-msc-2010-classification 03B15
    :name "03B15"
    :description "Higher-order logic and type theory"
    :parent 03Bxx)

(define-msc-2010-classification 03B20
    :name "03B20"
    :description "Subsystems of classical logic (including intuitionistic logic)"
    :parent 03Bxx)

(define-msc-2010-classification 03B22
    :name "03B22"
    :description "Abstract deductive systems"
    :parent 03Bxx)

(define-msc-2010-classification 03B25
    :name "03B25"
    :description "Decidability of theories and sets of sentences"
    :see-also '(11U05 12L05 20F10)
    :parent 03Bxx)

(define-msc-2010-classification 03B30
    :name "03B30"
    :description "Foundations of classical theories (including reverse mathematics)"
    :see-also '(03F35)
    :parent 03Bxx)

(define-msc-2010-classification 03B35
    :name "03B35"
    :description "Mechanization of proofs and logical operations"
    :see-also '(68T15)
    :parent 03Bxx)

(define-msc-2010-classification 03B40
    :name "03B40"
    :description "Combinatory logic and lambda-calculus"
    :see-also '(68N18)
    :parent 03Bxx)

(define-msc-2010-classification 03B42
    :name "03B42"
    :description "Logics of knowledge and belief (including belief change)"
    :parent 03Bxx)

(define-msc-2010-classification 03B44
    :name "03B44"
    :description "Temporal logic"
    :parent 03Bxx)

(define-msc-2010-classification 03B45
    :name "03B45"
    :description "Modal logic (including the logic of norms)"
    :see-also '(("Knowledge and belief" 03B42)
		("Temporal logic" 03B44)
		("Provability logic" 03F45))
    :parent 03Bxx)

(define-msc-2010-classification 03B47
    :name "03B47"
    :description "Substructural logics (including relevance, entailment, linear logic, Lambek calculus, BCK and BCI logics)"
    :see-also '(("Proof-theoretic aspects" 03F52))
    :parent 03Bxx)

(define-msc-2010-classification 03B48
    :name "03B48"
    :description "Probability and inductive logic"
    :see-also '(60A05)
    :parent 03Bxx)

(define-msc-2010-classification 03B50
    :name "03B50"
    :description "Many-valued logic"
    :parent 03Bxx)

(define-msc-2010-classification 03B52
    :name "03B52"
    :description "Fuzzy logic; logic of vagueness"
    :see-also '(68T27 68T37 94D05)
    :parent 03Bxx)

(define-msc-2010-classification 03B53
    :name "03B53"
    :description "Paraconsistent logics"
    :parent 03Bxx)

(define-msc-2010-classification 03B55
    :name "03B55"
    :description "Intermediate logics"
    :parent 03Bxx)

(define-msc-2010-classification 03B60
    :name "03B60"
    :description "Other nonclassical logic"
    :parent 03Bxx)

(define-msc-2010-classification 03B62
    :name "03B62"
    :description "Combined logics"
    :parent 03Bxx)

(define-msc-2010-classification 03B65
    :name "03B65"
    :description "Logic of natural languages"
    :see-also '(68T50 91F20)
    :parent 03Bxx)

(define-msc-2010-classification 03B70
    :name "03B70"
    :description "Logic in computer science"
    :see-also '(68-XX)
    :parent 03Bxx)

(define-msc-2010-classification 03B80
    :name "03B80"
    :description "Other applications of logic"
    :parent 03Bxx)

(define-msc-2010-classification 03B99
    :name "03B99"
    :description "None of the above, but in this section"
    :parent 03Bxx)

;; 03Cxx

(define-msc-2010-classification 03C05
    :name "03C05"
    :description "Equational classes, universal algebra"
    :see-also '(08Axx 08Bxx 18C05)
    :parent 03Cxx)

(define-msc-2010-classification 03C07
    :name "03C07"
    :description "Basic properties of first-order languages and structures"
    :parent 03Cxx)

(define-msc-2010-classification 03C10
    :name "03C10"
    :description "Quantifier elimination, model completeness and related topics"
    :parent 03Cxx)

(define-msc-2010-classification 03C13
    :name "03C13"
    :description "Finite structures"
    :see-also '(68Q15 68Q19])
    :parent 03Cxx)

(define-msc-2010-classification 03C15
    :name "03C15"
    :description "Denumerable structures"
    :parent 03Cxx)

(define-msc-2010-classification 03C20
    :name "03C20"
    :description "Ultraproducts and related constructions"
    :parent 03Cxx)

(define-msc-2010-classification 03C25
    :name "03C25"
    :description "Model-theoretic forcing"
    :parent 03Cxx)

(define-msc-2010-classification 03C30
    :name "03C30"
    :description "Other model constructions"
    :parent 03Cxx)

(define-msc-2010-classification 03C35
    :name "03C35"
    :description "Categoricity and completeness of theories"
    :parent 03Cxx)

(define-msc-2010-classification 03C40
    :name "03C40"
    :description "Interpolation, preservation, definability"
    :parent 03Cxx)

(define-msc-2010-classification 03C45
    :name "03C45"
    :description "Classification theory, stability and related concepts"
    :see-also '(03C48)
    :parent 03Cxx)

(define-msc-2010-classification 03C48
    :name "03C48"
    :description "Abstract elementary classes and related topics"
    :see-also '(03C45)
    :parent 03Cxx)

(define-msc-2010-classification 03C50
    :name "03C50"
    :description "Models with special properties (saturated, rigid, etc.)"
    :parent 03Cxx)

(define-msc-2010-classification 03C52
    :name "03C52"
    :description "Properties of classes of models"
    :parent 03Cxx)

(define-msc-2010-classification 03C55
    :name "03C55"
    :description "Set-theoretic model theory"
    :parent 03Cxx)

(define-msc-2010-classification 03C57
    :name "03C57"
    :description "Effective and recursion-theoretic model theory"
    :see-also '(03D45)
    :parent 03Cxx)

(define-msc-2010-classification 03C60
    :name "03C60"
    :description "Model-theoretic algebra"
    :see-also '(08C10 12Lxx 13L05)
    :parent 03Cxx)

(define-msc-2010-classification 03C62
    :name "03C62"
    :description "Models of arithmetic and set theory"
    :see-also '(03Hxx)
    :parent 03Cxx)

(define-msc-2010-classification 03C64
    :name "03C64"
    :description "Model theory of ordered structures; o-minimality"
    :parent 03Cxx)

(define-msc-2010-classification 03C65
    :name "03C65"
    :description "Models of other mathematical theories"
    :parent 03Cxx)

(define-msc-2010-classification 03C68
    :name "03C68"
    :description "Other classical first-order model theory"
    :parent 03Cxx)

(define-msc-2010-classification 03C70
    :name "03C70"
    :description "Logic on admissible sets"
    :parent 03Cxx)

(define-msc-2010-classification 03C75
    :name "03C75"
    :description "Other infinitary logic"
    :parent 03Cxx)

(define-msc-2010-classification 03C80
    :name "03C80"
    :description "Logic with extra quantifiers and operators"
    :see-also '(03B42 03B44 03B45 03B48)
    :parent 03Cxx)

(define-msc-2010-classification 03C85
    :name "03C85"
    :description "Second- and higher-order model theory"
    :parent 03Cxx)

(define-msc-2010-classification 03C90
    :name "03C90"
    :description "Nonclassical models (Boolean-valued, sheaf, etc.)"
    :parent 03Cxx)

(define-msc-2010-classification 03C95
    :name "03C95"
    :description "Abstract model theory"
    :parent 03Cxx)

(define-msc-2010-classification 03C98
    :name "03C98"
    :description "Applications of model theory"
    :see-also '(03C60)
    :parent 03Cxx)

(define-msc-2010-classification 03C99
    :name "03C99"
    :description "None of the above, but in this section"
    :parent 03Cxx)

;; 03Dxx

(define-msc-2010-classification 03D03
    :name 03D03
    :description "Thue and Post systems, etc."
    :parent 03Dxx)

(define-msc-2010-classification 03D05
    :name 03D05
    :description "Automata and formal grammars in connection with logical questions"
    :see-also '(68Q45 68Q70 68R15)
    :parent 03Dxx)

(define-msc-2010-classification 03D10
    :name 03D10
    :description "Turing machines and related notions"
    :see-also '(68Q05)
    :parent 03Dxx)

(define-msc-2010-classification 03D15
    :name 03D15
    :description "Complexity of computation (including implicit computational complexity)"
    :see-also '(68Q15 68Q17)
    :parent 03Dxx)

(define-msc-2010-classification 03D20
    :name 03D20
    :description "Recursive functions and relations, subrecursive hierarchies"
    :parent 03Dxx)

(define-msc-2010-classification 03D25
    :name 03D25
    :description "Recursively (computably) enumerable sets and degrees"
    :parent 03Dxx)

(define-msc-2010-classification 03D28
    :name 03D28
    :description "Other Turing degree structures"
    :parent 03Dxx)

(define-msc-2010-classification 03D30
    :name 03D30
    :description "Other degrees and reducibilities"
    :parent 03Dxx)

(define-msc-2010-classification 03D32
    :name 03D32
    :description "Algorithmic randomness and dimension"
    :see-also '(68Q30)
    :parent 03Dxx)

(define-msc-2010-classification 03D35
    :name 03D35
    :description "Undecidability and degrees of sets of sentences"
    :parent 03Dxx)

(define-msc-2010-classification 03D40
    :name 03D40
    :description "Word problems, etc."
    :see-also '(06B25 08A50 20F10 68R15)
    :parent 03Dxx)

(define-msc-2010-classification 03D45
    :name 03D45
    :description "Theory of numerations, effectively presented structures"
    :see-also '(("Intuitionistic and similar approaches" 03F55)
		03C57)
    :parent 03Dxx)

(define-msc-2010-classification 03D50
    :name 03D50
    :description "Recursive equivalence types of sets and structures, isols"
    :parent 03Dxx)

(define-msc-2010-classification 03D55
    :name 03D55
    :description "Hierarchies"
    :parent 03Dxx)

(define-msc-2010-classification 03D60
    :name 03D60
    :description "Computability and recursion theory on ordinals, admissible sets, etc."
    :parent 03Dxx)

(define-msc-2010-classification 03D65
    :name 03D65
    :description "Higher-type and set recursion theory"
    :parent 03Dxx)

(define-msc-2010-classification 03D70
    :name 03D70
    :description "Inductive definability"
    :parent 03Dxx)

(define-msc-2010-classification 03D75
    :name 03D75
    :description "Abstract and axiomatic computability and recursion theory"
    :parent 03Dxx)

(define-msc-2010-classification 03D78
    :name 03D78
    :description "Computation over the reals"
    :see-also '(("Constructive aspects" 03F60))
    :parent 03Dxx)

(define-msc-2010-classification 03D80
    :name 03D80
    :description "Applications of computability and recursion theory"
    :parent 03Dxx)

(define-msc-2010-classification 03D99
    :name 03D99
    :description "None of the above, but in this section"
    :parent 03Dxx)

;; 03Exx

(define-msc-2010-classification 03E02
    :name "03E02"
    :description "Partition relations"
    :parent 03Exx)

(define-msc-2010-classification 03E04
    :name "03E04"
    :description "Ordered sets and their cofinalities; pcf theory"
    :parent 03Exx)

(define-msc-2010-classification 03E05
    :name "03E05"
    :description "Other combinatorial set theory"
    :parent 03Exx)

(define-msc-2010-classification 03E10
    :name "03E10"
    :description "Ordinal and cardinal numbers"
    :parent 03Exx)

(define-msc-2010-classification 03E15
    :name "03E15"
    :description "Descriptive set theory"
    :see-also '(28A05 54H05)
    :parent 03Exx)

(define-msc-2010-classification 03E17
    :name "03E17"
    :description "Cardinal characteristics of the continuum"
    :parent 03Exx)

(define-msc-2010-classification 03E20
    :name "03E20"
    :description "Other classical set theory (including functions, relations, and set algebra)"
    :parent 03Exx)

(define-msc-2010-classification 03E25
    :name "03E25"
    :description "Axiom of choice and related propositions"
    :parent 03Exx)

(define-msc-2010-classification 03E30
    :name "03E30"
    :description "Axiomatics of classical set theory and its fragments"
    :parent 03Exx)

(define-msc-2010-classification 03E35
    :name "03E35"
    :description "Consistency and independence results"
    :parent 03Exx)

(define-msc-2010-classification 03E40
    :name "03E40"
    :description "Other aspects of forcing and Boolean-valued models"
    :parent 03Exx)

(define-msc-2010-classification 03E45
    :name "03E45"
    :description "Inner models, including constructibility, ordinal definability, and core models"
    :parent 03Exx)

(define-msc-2010-classification 03E47
    :name "03E47"
    :description "Other notions of set-theoretic definability"
    :parent 03Exx)

(define-msc-2010-classification 03E50
    :name "03E50"
    :description "Continuum hypothesis and Martin's axiom"
    :see-also '(03E57)
    :parent 03Exx)

(define-msc-2010-classification 03E55
    :name "03E55"
    :description "Large cardinals"
    :parent 03Exx)

(define-msc-2010-classification 03E57
    :name "03E57"
    :description "Generic absoluteness and forcing axioms"
    :see-also '(03E50)
    :parent 03Exx)

(define-msc-2010-classification 03E60
    :name "03E60"
    :description "Determinacy principles"
    :parent 03Exx)

(define-msc-2010-classification 03E65
    :name "03E65"
    :description "Other hypotheses and axioms"
    :parent 03Exx)

(define-msc-2010-classification 03E70
    :name "03E70"
    :description "Nonclassical and second-order set theories"
    :parent 03Exx)

(define-msc-2010-classification 03E72
    :name "03E72"
    :description "Fuzzy set theory"
    :parent 03Exx)

(define-msc-2010-classification 03E75
    :name "03E75"
    :description "Applications of set theory"
    :parent 03Exx)

(define-msc-2010-classification 03E99
    :name "03E99"
    :description "None of the above, but in this section"
    :parent 03Exx)

;; 03Fxx

(define-msc-2010-classification 03F03
    :name "03F03"
    :description "Proof theory, general"
    :parent 03Fxx)

(define-msc-2010-classification 03F05
    :name "03F05"
    :description "Cut-elimination and normal-form theorems"
    :parent 03Fxx)

(define-msc-2010-classification 03F07
    :name "03F07"
    :description "Structure of proofs"
    :parent 03Fxx)

(define-msc-2010-classification 03F10
    :name "03F10"
    :description "Functionals in proof theory"
    :parent 03Fxx)

(define-msc-2010-classification 03F15
    :name "03F15"
    :description "Recursive ordinals and ordinal notations"
    :parent 03Fxx)

(define-msc-2010-classification 03F20
    :name "03F20"
    :description "Complexity of proofs"
    :parent 03Fxx)

(define-msc-2010-classification 03F25
    :name "03F25"
    :description "Relative consistency and interpretations"
    :parent 03Fxx)

(define-msc-2010-classification 03F30
    :name "03F30"
    :description "First-order arithmetic and fragments"
    :parent 03Fxx)

(define-msc-2010-classification 03F35
    :name "03F35"
    :description "Second- and higher-order arithmetic and fragments"
    :see-also '(03B30)
    :parent 03Fxx)

(define-msc-2010-classification 03F40
    :name "03F40"
    :description "Gödel numberings in proof theory and issues of incompleteness"
    :parent 03Fxx)

(define-msc-2010-classification 03F45
    :name "03F45"
    :description "Provability logics and related algebras (e.g., diagonalizable algebras)"
    :see-also '(03B45 03G25 06E25)
    :parent 03Fxx)

(define-msc-2010-classification 03F50
    :name "03F50"
    :description "Metamathematics of constructive systems"
    :parent 03Fxx)

(define-msc-2010-classification 03F52
    :name "03F52"
    :description "Linear logic and other substructural logics"
    :see-also '(03B47)
    :parent 03Fxx)

(define-msc-2010-classification 03F55
    :name "03F55"
    :description "Intuitionistic mathematics"
    :parent 03Fxx)

(define-msc-2010-classification 03F60
    :name "03F60"
    :description "Constructive and recursive analysis"
    :see-also '(03B30 03D45 03D78 26E40 46S30 47S30)
    :parent 03Fxx)

(define-msc-2010-classification 03F65
    :name "03F65"
    :description "Other constructive mathematics"
    :see-also '(03D45)
    :parent 03Fxx)

(define-msc-2010-classification 03F99
    :name "03F99"
    :description "None of the above, but in this section"
    :parent 03Fxx)

;; 03Gxx

(define-msc-2010-classification 03G05
    :name "03G05"
    :description "Boolean algebras"
    :see-also '(06Exx)
    :parent 03Gxx)

(define-msc-2010-classification 03G10
    :name "03G10"
    :description "Lattices and related structures"
    :see-also '(06Bxx)
    :parent 03Gxx)

(define-msc-2010-classification 03G12
    :name "03G12"
    :description "Quantum logic"
    :see-also '(06C15 81P10)
    :parent 03Gxx)

(define-msc-2010-classification 03G15
    :name "03G15"
    :description "Cylindric and polyadic algebras; relation algebras"
    :parent 03Gxx)

(define-msc-2010-classification 03G20
    :name "03G20"
    :description "Łukasiewicz and Post algebras"
    :see-also '(06D25 06D30)
    :parent 03Gxx)

(define-msc-2010-classification 03G25
    :name "03G25"
    :description "Other algebras related to logic"
    :see-also '(03F45 06D20 06E25 06F35)
    :parent 03Gxx)

(define-msc-2010-classification 03G27
    :name "03G27"
    :description "Abstract algebraic logic"
    :parent 03Gxx)

(define-msc-2010-classification 03G30
    :name "03G30"
    :description "Categorical logic, topoi"
    :see-also '(18B25 18C05 18C10)
    :parent 03Gxx)

(define-msc-2010-classification 03G99
    :name "03G99"
    :description "None of the above, but in this section"
    :parent 03Gxx)

;; 03Hxx

(define-msc-2010-classification 03H05
    :name "03H05"
    :description "Nonstandard models in mathematics"
    :see-also '(26E35 28E05 30G06 46S20 47S20 54J05)
    :paent 03Hxx)

(define-msc-2010-classification 03H10
    :name "03H10"
    :description "Other applications of nonstandard models (economics, physics, etc.)"
    :paent 03Hxx)

(define-msc-2010-classification 03H15
    :name "03H15"
    :description "Nonstandard models of arithmetic"
    :see-also '(11U10 12L15 13L05)
    :paent 03Hxx)

(define-msc-2010-classification 03H99
    :name "03H99"
    :description "None of the above, but in this section"
    :paent 03Hxx)
