import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import {Observable, Subject, tap} from 'rxjs';
import {Router} from "@angular/router";
import { TokenDTO } from 'src/dto/tokenDTO';
import * as moment from "moment/moment";
import {jwtDecode} from "jwt-decode";
import {environment} from "../../environments/environment";

@Injectable({
  providedIn: 'root'
})
export class AuthService {

    private AUTH_API_URL = environment.C_SHARP;

    login$ = new Subject<boolean>();

    constructor(private http: HttpClient, private router: Router) { }

    getRoles(): Observable<string[]> {

      return this.http.get<string[]>('http://localhost:4000/api/roles');
    }

    public login(email:string, password:string,  showSpinner?: boolean) {
      return this.http.post<TokenDTO>(
        `${this.AUTH_API_URL}/api/users/authenticate`,
        {email, password},
        {reportProgress: showSpinner})
        .pipe(
          tap(res => this.createSession(res))
        )

    }

    private createSession(authResult: TokenDTO) {
      const expiresAt = moment().add(authResult.expiresIn,'seconds');
      localStorage.setItem('token', authResult.accessToken);
      localStorage.setItem('expires_at', JSON.stringify(expiresAt.valueOf()) );
    }

    public isLoggedIn() {
      return moment().isBefore(this.tokenExpiration());
    }

    public logout(): void {
      localStorage.removeItem('token');
      localStorage.removeItem('expires_at')
      this.router.navigate(['login']);
    }

    public userRole() {
      const token = localStorage.getItem("token") ?? '';
      const tokenInfo = this.decodeAccessToken(token);
      return tokenInfo?.role;
    }

    private decodeAccessToken(token: string): any {
      try {
        return jwtDecode(token);
      } catch(Error) {
        return null;
      }
    }

    private tokenExpiration() {
      const expiration = localStorage.getItem("expires_at") ?? '1';
      const expiresAt = JSON.parse(expiration);
      return moment(expiresAt);
    }
}
