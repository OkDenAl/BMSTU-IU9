package apperrors

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
