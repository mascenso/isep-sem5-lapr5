import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

@Injectable()
export class loginService {

    constructor(private http: HttpClient) { }

    getRoles(): Observable<string[]> {
      
      return this.http.get<string[]>('http://localhost:4000/api/roles');
    }
}