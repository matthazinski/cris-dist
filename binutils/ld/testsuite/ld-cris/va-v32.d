# source: start1.s --march=v32
# source: move-1.s --march=common_v10_v32
# ld:
# objdump: -p

# Test that linking a v10+v32 compatible object to a v32 object
# does work and results in the output marked as a v32 object.

#...
private flags = 3: \[symbols have a _ prefix\] \[v32\]
#pass