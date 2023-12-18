import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { environment } from '../../environments/environment';
import { UserResponseDTO, CreateUserRequestDTO } from '../../dto/userDTO';

@Injectable({
  providedIn: 'root'
})
export class UserService {

  private API_URL = environment.C_SHARP;

  constructor(private http: HttpClient) { }

  public registerUser(user: CreateUserRequestDTO, showSpinner?: boolean): Observable<UserResponseDTO> {
    const headers = new HttpHeaders({
        'Content-Type': 'application/json',
        // Add any additional headers as needed
    });

    const options = {
        headers: headers,
        reportProgress: showSpinner
        // Other options if needed
    };

    return this.http.post<UserResponseDTO>(`${this.API_URL}/api/users/`, user, options);
}


  public getAllUsers(): Observable<UserResponseDTO[]> {
    return this.http.get<UserResponseDTO[]>(`${this.API_URL}/api/users/`);
  }

  public updateUser(user: CreateUserRequestDTO): Observable<UserResponseDTO> {
    return this.http.patch<UserResponseDTO>(`${this.API_URL}/api/users/`, user);
  }

}
