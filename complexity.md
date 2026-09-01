# Time complexity 
### Size units
t : tape length

---
# Final complexity : best O(1) || worst O(n)
``` 
    best O(1) || worst O(t) 
                |
                v
    best O(1) || worst O(n)
```
---
### start_machine O(1) || worst O(t) :
```
    go best O(1) || worst O(t) :
        O(1)                                   Hashtable lookup
        execute_cell best O(1) || worst O(t)
```
### execute_cell best O(1) || worst O(t) :
``` 
    get_transition O(1)
    print_step O(1)
    check_loop O(1)
    |> check_bounds best O(1) || worst O(t)
    |> write_cell best O(1) || worst O(t)
```
### get_transition O(1) : 
``` 
    O(1)        Hashtable lookup
    |> O(1)     Hashtable lookup
```
### print_step O(1) :
``` 
    print_tape O(1)
```
### print_tape O(1) :
``` 
    tape_to_window_str O(1)
```
### tape_to_window_str O(1) : 
``` 
    O(1)
```
### check_loop O(1) :
``` 
    O(1)
```
### check_bounds best O(1) || worst O(t) : 
``` 
    best O(1) || worst O(t)
```
### write_cell best O(1) || worst O(t) : 
``` 
    best O(1) || worst O(t)
```
