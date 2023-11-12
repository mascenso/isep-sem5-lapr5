import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import {Observable, Subject} from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class LoginService {

    private isLoggedIn = false;

    login$ = new Subject<boolean>();

    constructor(private http: HttpClient) { }

    getRoles(): Observable<string[]> {

      return this.http.get<string[]>('http://localhost:4000/api/roles');
    }

    public login(): void {
      this.login$.next(true);
      this.isLoggedIn = true;
    }

    public isLoggedId(): boolean {
      return this.isLoggedIn;
    }
}
