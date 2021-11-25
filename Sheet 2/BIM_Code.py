# -*- coding: utf-8 -*-
"""
Created on Sun Oct 10 10:04:11 2021

@author: Admin
"""
import numpy as np
#%% Algorithm 4: Set of disjoint strings

def disjoint_string(F: list) -> list:
    dis_str = []
    for s in F:
        s_super = []
        for e in F:
            if s in e:
                s_super.append(e)
        if len(s_super) == 1:
            dis_str.append(s_super[0])
                       
    return dis_str

#%% Algorithm 5: Greedy-Superstring

def GreedySuperstring(F: list) -> str:
    dis_str = disjoint_string(F)
    
    
#%% Algorithm 14.2: Optimal Alignment via Dynamic Programming

def score(a,b):
    return 1 if a==b else -1
    
def a_rec(s,t,i,j):
    g = score(s[i], t[j])
    if i+j == 0:
        return g
    elif i == 0:
        return a_rec(s,t,i,j-1) -2
    elif j == 0:
        return a_rec(s,t,i-1,j) -2
    else:
        return max(a_rec(s,t,i-1,j) -2, a_rec(s,t,i,j-1) -2, a_rec(s,t,i-1,j-1) +g)

def opt_alignment(s,t):
    n_row, n_col = len(s), len(t)
    M = np.zeros((n_row, n_col))
    for i in range(n_row):
        for j in range(n_col):
                g = score(s[i], t[j])
                if i+j == 0:
                    M[0,0] = g
                elif i == 0:
                    M[i,j] = M[i,j-1] -2
                elif j == 0:
                    M[i,j] = M[i-1,j] -2
                else:
                     M[i,j] = max(M[i-1,j] -2,  M[i,j-1] -2,  M[i-1,j-1] + g)
                     
    return M[-1,-1]
                    
s = "ACTTATGCCTT"
t = "ACAGGCCTCT"

print(opt_alignment(s,t))
    
















