# SAT
This program implements a solver for the boolean satisfiability problem. The following operations are supported:

not: ~  
and: &  
or: |  

To run:
```bash
./main "1 & (2 | ~3)"
```

# Changelog
4/10/21: implemented evaluation of boolean functions for specific inputs  
4/11/21: implemented verification of satisfiability  
4/12/21: fixed error in count_vars; updated tests  
4/13/21: added lexer  
4/16/21: added parser  
4/17/21: end-to-end solver complete! added Makefile  
4/19/21: fixed formatting issue when reporting failure
