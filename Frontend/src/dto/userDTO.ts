export interface UserResponseDTO {
  id: string;
  email: string;
  password: string,
  firstName: string,
  lastName: string;
  role: string;
  active: boolean;
}

export interface CreateUserRequestDTO {
  email: string;
  password: string;
  firstName: string;
  lastName: string;
  role: string;
  active: boolean; 
}
