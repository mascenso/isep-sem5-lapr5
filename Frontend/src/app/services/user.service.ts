import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../environments/environment';
import { UserResponseDTO, CreateUserRequestDTO } from '../../dto/userDTO';
import {PatchUserDataRequestDTO} from "../../dto/patchUserDataRequestDTO";

@Injectable({
  providedIn: 'root'
})
export class UserService {

  private API_URL = environment.C_SHARP;

  constructor(private http: HttpClient) { }

  public registerUser(user: CreateUserRequestDTO, showSpinner?: boolean): Observable<UserResponseDTO> {
    return this.http.post<UserResponseDTO>(`${this.API_URL}/api/users/register-system-user`, user);
  }

  public GetInactiveUsers(): Observable<UserResponseDTO[]> {
    return this.http.get<UserResponseDTO[]>(`${this.API_URL}/api/users/inactive`);
  }

  public updateUser(user: PatchUserDataRequestDTO): Observable<UserResponseDTO> {
    return this.http.patch<UserResponseDTO>(`${this.API_URL}/api/users/patch-user`, user);
  }

  public getUserData(showSpinner?: boolean): Observable<UserResponseDTO> {
    return this.http.get<UserResponseDTO>(`${this.API_URL}/api/users/user`);
  }

  public deleteUser(showSpinner?: boolean): Observable<any> {
    return this.http.delete(`${this.API_URL}/api/users/delete-user`);
  }

}
