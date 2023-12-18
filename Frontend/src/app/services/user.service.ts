import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../environments/environment';
import { UserResponseDTO, CreateUserRequestDTO } from '../../dto/userDTO';

@Injectable({
  providedIn: 'root'
})
export class UserService {

  private API_URL = 'http://localhost:5000';

  constructor(private http: HttpClient) { }

  public registerUser(user: CreateUserRequestDTO, showSpinner?: boolean): Observable<UserResponseDTO> {
    return this.http.post<UserResponseDTO>(`${this.API_URL}/api/users`, user, { reportProgress: showSpinner });
  }

  public getAllUsers(): Observable<UserResponseDTO[]> {
    return this.http.get<UserResponseDTO[]>(`${this.API_URL}/api/users`);
  }

  public updateUser(user: CreateUserRequestDTO): Observable<UserResponseDTO> {
    return this.http.patch<UserResponseDTO>(`${this.API_URL}/api/users`, user);
  }

}
