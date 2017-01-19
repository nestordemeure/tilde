# tilde

the current version is just a proof of concept, it :
- is not designed for speed
- is not as clean as it could be (the Exist/Forall distinction could be more explicit in the code)
- does not implement a great decision tree (greedy C4.5 heuristics, no pruning/etc)
- is not equipped to directly import a database : the user need to handwrite the data and its properties
- uses properties of a set of parts of the sample
  (properties on the wagons of a train, the parts of a car, etc)
  not properties of the sample as a whole 
  (not the number of wagons)
  not properties on more than one set of parts 
  (not the patient of a hospital AND the services of a hospital)