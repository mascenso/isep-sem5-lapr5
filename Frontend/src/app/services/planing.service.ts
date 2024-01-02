import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, catchError, tap } from 'rxjs';
import { environment } from 'src/environments/environment';
import { Conditional } from '@angular/compiler';
import { RobotDTO } from 'src/dto/robotDTO';

@Injectable({
  providedIn: 'root'
})
export class PlaningService {

  private API_URL = environment.API_URL_TASKS;

  constructor(private http: HttpClient) { }

  calcular(piso1: string, piso2: string): Observable<any> {
    return this.http.get<any>(`${this.API_URL}/api/route/routePlaning/${piso1}/${piso2}`);
  }

  planear(taskInfo: any): Observable<any> {
    
    return this.http.post<any>(`${this.API_URL}/api/tasks/planning/`,taskInfo);
   /* return this.http.post<any>(`${this.API_URL}/api/tasks/planning/`, taskInfo).pipe(
      tap((response: Record<string, any>) => {
        console.log("Resposta do servidor:", response);
      }),
      catchError((error) => {
        console.error("Erro na solicitação:", error);
        throw error; // lança o erro para ser tratado pelo chamador do método
      })
    ); */
  }

  public getAllRobots(): Observable<RobotDTO[]> {
    return this.http.get<RobotDTO[]>(`${this.API_URL}/api/robots/`);
  }


  }


