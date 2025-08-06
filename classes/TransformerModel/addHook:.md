# addHook: hook

**Purpose**: Registers a hook to be executed during forward passes for analysis or intervention purposes.

**Parameters**:
- `hook` (Hook): A Hook instance specifying when and how to intercept model execution

**Return Value**: The hook instance (for method chaining)

**Side Effects**: 
- Adds hook to the model's hook manager
- Hook will be executed on subsequent forward passes
- May impact model performance depending on hook complexity

**Usage Examples**:
```smalltalk
"Add activation extraction hook"
activationHook := ActivationHook 
    layer: 8 
    component: #attention
    action: [:activation | activation copy].
model addHook: activationHook.

"Add intervention hook to zero specific attention heads"
interventionHook := InterventionHook
    layer: 10
    component: #attention  
    action: [:activation | activation zeroHeads: #(2 5 8)].
model addHook: interventionHook.

"Add probe hook for linear probe training"
probeHook := ProbeHook
    layer: 12
    component: #residual
    probe: myLinearProbe.
model addHook: probeHook.

"Method chaining for multiple hooks"
model 
    addHook: (ActivationHook layer: 6 component: #mlp);
    addHook: (CachingHook layer: 6 component: #residual);
    addHook: (InterventionHook layer: 8 component: #attention).
```

**Performance Notes**: Each hook adds computational overhead. Remove unused hooks with `removeHook:` or `clearHooks` to maintain performance.