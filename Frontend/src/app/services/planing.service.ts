import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from 'src/environments/environment';
import { Conditional } from '@angular/compiler';

@Injectable({
  providedIn: 'root'
})
export class PlaningService {

  private API_URL = environment.API_URL_TASKS;

  constructor(private http: HttpClient) { }

  calcular(piso1: string, piso2: string): Observable<any> {
    return this.http.get<any>(`${this.API_URL}/api/route/routePlaning/${piso1}/${piso2}`);
  }


  /* //ESTE FUNCIONA MAS ESTÁ A LIGAR DIRETAMENTE A UI AO PLANEMANENTO
   planear(taskInfo: any): Observable<any> {
     //const url = `http://vs770.dei.isep.ipp.pt:8082/tarefas?ng=${Ngeracoes}&dp=${dimensaoPop}&p1=${pobCruz}&p2=${pobMut}&t=${tempoLimite}&av=${avaliacaoDef}&nestab=${nEstabiliz}`;
     //const url = `http://rdg-planning.h5b0bhc4e5a0dddx.westeurope.azurecontainer.io/tarefas?ng=${Ngeracoes}&dp=${dimensaoPop}&p1=${pobCruz}&p2=${pobMut}&t=${tempoLimite}&av=${avaliacaoDef}&nestab=${nEstabiliz}`;
     const url = `http://localhost:8082/tarefas?ng=${taskInfo.Ngeracoes}&dp=${taskInfo.dimensaoPop}&p1=${taskInfo.pobCruz}&p2=${taskInfo.pobMut}&t=${taskInfo.tempoLimite}&av=${taskInfo.avaliacaoDef}&nestab=${taskInfo.nEstabiliz}`;
     return this.http.get(url, {responseType: 'text'});
   }
 */

  planear(taskInfo: any): Observable<any> {
    console.log("AQUI");
    // const url = `${this.API_URL}/api/tasks/planning`; 

    return this.http.post<any>(`${this.API_URL}/api/tasks/planning/`,taskInfo);
  }

}
