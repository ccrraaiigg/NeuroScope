# clearHooks

**Purpose**: Removes all registered hooks from the model, returning it to standard execution mode.

**Return Value**: Self (for method chaining)

**Side Effects**: All hooks are unregistered and will no longer execute during forward passes

**Usage Examples**:
```smalltalk
"Clean slate for new analysis"
model clearHooks.

"Reset after intervention experiment"
model 
    clearHooks;
    forward: tokens.  "Clean forward pass"
```