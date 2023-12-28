import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class PlaningService {
  constructor(private http: HttpClient) {}

  calcular(piso1: string, piso2: string): Observable<any> {
    const url = `http://localhost:8082/caminho?pisoOrigem=${piso1}&pisoDestino=${piso2}`;
    return this.http.get(url);
  }

  planear(Ngeracoes: number, dimensaoPop: number,pobCruz: number,  pobMut: number, tempoLimite: number,avaliacaoDef: number,nEstabiliz : number): Observable<any> {
    const url = `http://localhost:8082/tarefas?NG=${Ngeracoes}&Dp=${dimensaoPop}&P1=${pobCruz}&P2=${pobMut}&T=${tempoLimite}&Av=${avaliacaoDef}&NEstab=${nEstabiliz}`;
    return this.http.get(url);
  }

}