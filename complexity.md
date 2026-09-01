# Time complexity 
### Size units
t : tape length

---
# Final complexity : O(1)
``` 
    O(1)
     |
     v
    O(1)
```
---
### start_machine O(1) :
```
    go O(1) :
        O(1)              Hashtable lookup
        execute_cell O(1)
```
### execute_cell O(1) :
``` 
    get_transition O(1)   Hashtable lookup
    print_step O(1)
    check_loop O(1)
    |> check_bounds O(1)
    |> write_cell O(1)
```
### get_transition O(1) : 
``` 
    O(1)                  Hashtable lookup
    |> O(1)               Hashtable lookup
```
### print_step O(1) :
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
### check_bounds O(1) : 
``` 
    O(1)
```
### write_cell O(1) : 
``` 
    O(1)
```
