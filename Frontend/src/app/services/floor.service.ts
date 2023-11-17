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

}
