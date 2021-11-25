# -*- coding: utf-8 -*-
"""
Created on Sat Nov 13 11:20:08 2021

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

# Helper
def find_overlap(frag1, frag2):
        for i in range(len(frag1)-1,0,-1):
            # Beginning frag1 matches end frag2
            if frag1[:i] == frag2[-i:]:
                return i
            # End frag1 matches beginning frag2.
            elif frag1[-i:] == frag2[:i]:
                return -i
            
        return 0

def GreedySuperstring(F: list) -> str:
    dis_str = disjoint_string(F)
    n_frags = len(dis_str)
    Overlaps = np.zeros((n_frags, n_frags))
    for i in range(n_frags):
        frag1 = dis_str[i]
        for j in range(i+1,n_frags):
            frag2 = dis_str[j]
            Overlaps[i,j] = find_overlap(frag1,frag2)
                
    for i in range(n_frags+1):  

        if np.max(np.abs(Overlaps)) == 0:
            return "".join(dis_str)
        
        max_overlap_pos = np.argmax(np.abs(Overlaps))
        merge_idx1, merge_idx2 = max_overlap_pos // Overlaps.shape[0], max_overlap_pos % Overlaps.shape[1]
               
        m1 = dis_str[merge_idx1]
        m2 = dis_str[merge_idx2]
        
        dis_str[merge_idx1] = ""
        dis_str[merge_idx2] = ""
        
        max_overlap = int(Overlaps[merge_idx1, merge_idx2])
        
        if max_overlap < 0:
            merged_string = m1[:max_overlap] + m2
            
        else:
            merged_string =  m2 + m1[max_overlap:]
            
        dis_str[merge_idx1] = merged_string
         
        for j in range(merge_idx1+1, len(Overlaps)):
                Overlaps[merge_idx1,j] = find_overlap(merged_string, dis_str[j])
        
        for i in range(merge_idx1):
                Overlaps[i,merge_idx1] = find_overlap(dis_str[i], merged_string)
                
        for j in range(merge_idx2, len(Overlaps)):
                Overlaps[merge_idx2,j] = 0
        
        for i in range(merge_idx2):
                Overlaps[i,merge_idx2] = 0
               
        Overlaps[merge_idx1,merge_idx1] = 0
        
    return dis_str
                


if __name__ == "__main__":
    
    # Example from the lecture notes (p.52)
    F = ["AG", "TATACG", "GAAC", "TACAGCT", "TATGCTATTAT", "C", "AGTATACGG", "AACTACA", "GC", "T",
         "TATGCT", "ATTA", "TC", "AGTATAC", "GG", "AACT", "ACAGC", "TTATG", "CTATTATC", "AGT", "ATACGGAAC", "TA",
         "CAGCTT", "ATGCTATT", "A", "TC", "AGTATACGGAACT", "A", "CAGCTTA", "TGCTATT", "A", "T", "C"]    

    superstring = GreedySuperstring(F)             

    print(f"\nLength of the superstring: {len(superstring)}.\n")
    print(f"Length of the concatenation of all strings in L: {len(''.join(L))}.")
    
    # 1.2)
    with open('Textfragmente.txt', "r") as f:
        lines = f.read().splitlines()
    
    text = GreedySuperstring(lines)
    