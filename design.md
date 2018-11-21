RTL interface consists of the following layers:
1 Register value generation. ie: knowing how to generate parameters assigned to registers
2 Register value generation. ie. knowing how these values are arranged in register bits.
3 Register addressing. ie: Knowing where in (relative) memory these values should be placed.
4 Register protocol. ie: Knowing how to signal to the hardware that registers have been set.

An rtl generator should be able to handle steps 2 and 3.



