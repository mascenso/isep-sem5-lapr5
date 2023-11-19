import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import {Observable, of, Subject} from 'rxjs';
import {Router} from "@angular/router";

@Injectable({
  providedIn: 'root'
})
export class AuthService {

    private isLoggedIn = false;

    login$ = new Subject<boolean>();

    constructor(private http: HttpClient, private router: Router) { }

    getRoles(): Observable<string[]> {

      return this.http.get<string[]>('http://localhost:4000/api/roles');
    }

    public login(email:string, password:string, role:string): Observable<any> {
      this.isLoggedIn = true;
      return of({
        firstName: "Manel",
        lastName: "da Maquina",
        email: email,
        role: role
      });
    }

    public isLoggedId(): boolean {
      return this.isLoggedIn;
    }

    public logout(): void {
      this.isLoggedIn = false;
      localStorage.removeItem('token');
      localStorage.removeItem('role');
      this.router.navigate(['login']);
    }
}
