package apperrors

var (
	BadRequest    = NewAppError("bad request", "US-0000")
	ErrBodyEncode = NewAppError("body encode failed", "US-0001")
	NotFound      = NewAppError("not found", "US-0003")
)

type AppError struct {
	message string
	code    string
}

func NewAppError(message string, code string) *AppError {
	return &AppError{
		message: message,
		code:    code,
	}
}

func (a *AppError) Error() string {
	return a.message
}

func (a *AppError) GetCode() string {
	return a.code
}
