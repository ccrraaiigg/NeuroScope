isSupportedArchitecture: config
	"Checks if the model architecture specified in the config is supported by NeuroScope.
	
	Parameters:
	- config (Dictionary): The model configuration dictionary
	
	Returns: Boolean indicating whether the architecture is supported"
	
	| architectures supportedArchitectures |
	
	"Define supported model architectures"
	supportedArchitectures := Set withAll: #(
		'GPT2LMHeadModel'
		'GPTNeoForCausalLM' 
		'GPTJForCausalLM'
		'LlamaForCausalLM'
		'BertModel'
		'BertForMaskedLM'
		'RobertaModel'
		'RobertaForMaskedLM'
	).
	
	"Extract architectures from config"
	architectures := config at: #architectures ifAbsent: [#()].
	architectures := architectures asSet.
	
	"Check if any architecture is supported"
	^(architectures intersection: supportedArchitectures) isNotEmpty