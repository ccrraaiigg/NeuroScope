# removeHook: hook

**Purpose**: Unregisters a previously added hook from the model's execution pipeline.

**Parameters**:
- `hook` (Hook): The hook instance to remove

**Return Value**: Boolean indicating whether the hook was found and removed

**Usage Examples**:
```smalltalk
"Remove specific hook"
success := model removeHook: myActivationHook.

"Remove hook by name"
model removeHookNamed: 'layer8-attention-extraction'.

"Conditional hook removal"
model hooks do: [:hook |
    hook layer = 6 ifTrue: [model removeHook: hook]
].
```