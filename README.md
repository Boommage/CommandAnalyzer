# Command Analyzer
A Scala program that analyzes and processes text-based commands. The program contains a list of grammar patterns (commands) along with a lit of objects, each with a description and category (e.g., item, container, direction). The extraction of adjectives, nouns, kinds, verbs, prepositions, and action types from both grammar and object descriptions were written with declaritive programing in mind; Returning sorted, distinct listsMost functions. 
# What I Learned
* Declarative programming principles, focusing on what the code should do rather than how.
* Scala’s powerful built-in collection functions (map, filter, flatMap, foldLeft, etc.) for concise and expressive data processing.
* How to tokenize and analyze user input through structured parsing and pattern recognition.
* How to use case classes to cleanly represent complex data like game objects and grammar rules.
* The importance of clean, deterministic outputs using sorting and deduplication to maintain consistent behavior.
* How to validate natural language commands against formal grammar patterns and world models using functional composition.
