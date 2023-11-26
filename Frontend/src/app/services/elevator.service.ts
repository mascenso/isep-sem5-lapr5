import { Injectable } from '@angular/core';
import {Observable} from "rxjs";
import {HttpClient} from "@angular/common/http";
import {environment} from "../../environments/environment";
import { ElevatorResponseDTO, CreateElevatorDTO } from "../../dto/elevatorDTO";

@Injectable({
  providedIn: 'root'
})
export class ElevatorService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) { }

  public createElevator(elevator: CreateElevatorDTO, showSpinner?: boolean): Observable<ElevatorResponseDTO> {
    return this.http.post<ElevatorResponseDTO>(`${this.API_URL}/api/elevators`, elevator, {reportProgress: showSpinner});
  }

  public getBuildingElevators(buildingId: string, showSpinner?:boolean):Observable<ElevatorResponseDTO> {
    return this.http.get<ElevatorResponseDTO>(`${this.API_URL}/api/elevators/${buildingId}`, { reportProgress: showSpinner });
  }

}
