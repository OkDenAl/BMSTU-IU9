package protocol

type Response struct {
	Answer *string `json:"answer"`
	Err    *Error  `json:"error"`
}

type Error struct {
	Message string `json:"message"`
	Code    *int   `json:"code"`
}

type Status struct {
	IsCpp bool
}
