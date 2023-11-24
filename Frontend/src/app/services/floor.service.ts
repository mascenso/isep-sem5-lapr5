import { Injectable } from '@angular/core';
import {environment} from "../../environments/environment";
import {HttpClient} from "@angular/common/http";
import {Observable} from "rxjs";
import { FloorResponseDTO } from "../../dto/floorDTO";

@Injectable({
  providedIn: 'root'
})
export class FloorService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) { }

  public getFloorsAtBuildings(buildingId: string, showSpinner?:boolean): Observable<FloorResponseDTO[]> {
    return this.http.get<FloorResponseDTO[]>(`${this.API_URL}/api/floors/buildings?building=${buildingId}`, {reportProgress: showSpinner});
  }

  public getFloorsWithElevatorByBuildingId(buildingId: string, showSpinner?:boolean): Observable<FloorResponseDTO[]> {
    return this.http.get<FloorResponseDTO[]>(`${this.API_URL}/api/floors/buildings/${buildingId}/with-elevator`, {reportProgress: showSpinner});
  }

  //update is a patch
  public updateFloor(floor: FloorResponseDTO): Observable<FloorResponseDTO[]> {
    return this.http.patch<FloorResponseDTO[]>(`${this.API_URL}/api/floors`, floor);
  }

  //edit is a put
  public editFloor(floor: FloorResponseDTO): Observable<FloorResponseDTO[]> {
    return this.http.put<FloorResponseDTO[]>(`${this.API_URL}/api/floors`, floor);
  }

  public createFloor(floor: FloorResponseDTO): Observable<FloorResponseDTO[]> {
    return this.http.post<FloorResponseDTO[]>(`${this.API_URL}/api/floors`, floor);
  }

  public addMapFloor(floor: string, map: any): Observable<FloorResponseDTO[]> {
    const file: File = map.target.files[0];
    const content = {};
  
    if (file) {
      const reader = new FileReader();
  
      // Crie um novo Observable para controlar a operação assíncrona
      return new Observable(observer => {
        reader.onload = () => {
          try {
            const content = JSON.parse(reader.result as string);
            
            // Use o método post e retorne o Observable resultante
            this.http.post<FloorResponseDTO[]>(`${this.API_URL}/api/floors/${floor}/map`, {floorMap:content})
              .subscribe(
                (response: FloorResponseDTO[]) => {
                  // Emite o resultado para observadores
                  observer.next(response);
                  observer.complete();
                },
                error => {
                  console.error('Erro ao fazer a solicitação HTTP:', error);
                  observer.error(error);
                }
              );
          } catch (error) {
            console.error('Erro ao analisar o JSON:', error);
            observer.error(error);
          }
        };
  
        reader.readAsText(file);
      });
    }
  
    // Se nenhum arquivo foi fornecido, retorna um Observable vazio
    return new Observable(observer => observer.complete());
  }

}
