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
    });

    const options = {
        headers: headers,
        reportProgress: showSpinner
    };

    console.log(user);
    const url = `http://localhost:5000/api/users/`;

    return this.http.post<UserResponseDTO>(`${this.API_URL}/api/users/register`, user, options);
    //return this.http.post<UserResponseDTO>(url, user);
  }

  public GetInactiveUsers(): Observable<UserResponseDTO[]> {
    return this.http.get<UserResponseDTO[]>(`${this.API_URL}/api/users/inactive`);
  }

  /*
  public getAllUsers(): Observable<UserResponseDTO[]> {
    return this.http.get<UserResponseDTO[]>(`${this.API_URL}/api/users/`);
  }
  */

  /*
  public updateUser(user: CreateUserRequestDTO): Observable<UserResponseDTO> {
    return this.http.patch<UserResponseDTO>(`${this.API_URL}/api/users/patch-user`, user);
  }
  */

  
  public updateUser(userId: string, user: Partial<CreateUserRequestDTO>): Observable<UserResponseDTO> {
    return this.http.patch<UserResponseDTO>(`${this.API_URL}/api/users/${userId}`, user);
  }
  
  

  public deleteUser(userId: string): Observable<UserResponseDTO> {
    return this.http.delete<UserResponseDTO>(`${this.API_URL}/api/users/${userId}`);
  }

}
