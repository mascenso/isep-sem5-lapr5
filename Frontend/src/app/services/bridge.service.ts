import { Injectable } from '@angular/core';
import { catchError, Observable, throwError } from "rxjs";
import { HttpClient, HttpErrorResponse } from "@angular/common/http";
import {environment} from "../../environments/environment";
import { BridgeDTO, BridgeResponseDTO, CreateBridgeRequestDTO } from "../../dto/bridgeDTO";
import { BuildingResponseDTO } from "../../dto/buildingDTO";
import { FloorResponseDTO } from "../../dto/floorDTO";
import { retry } from "rxjs/operators";


@Injectable({
  providedIn: 'root'
})
export class BridgeService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) { }

  public createBridge(bridge: CreateBridgeRequestDTO, showSpinner?: boolean): Observable<BridgeResponseDTO> {
      const url = `${this.API_URL}/api/bridges`;
      return this.http.post<BridgeResponseDTO>(url, bridge, {reportProgress: showSpinner})
        .pipe(
          retry(1),
          catchError((error: HttpErrorResponse) => {
            return throwError(error);
          })
        );
    }

  public getAllBridges(): Observable<BridgeResponseDTO[]> {
    return this.http.get<BridgeResponseDTO[]>(`${this.API_URL}/api/bridges`);
  }

  getAllBuildings() : Observable<BuildingResponseDTO[]> {
      return this.http.get<BuildingResponseDTO[]>(`${this.API_URL}/api/buildings`);
  }

  getFloorsByBuildingId($event: any, b: boolean) : Observable<FloorResponseDTO[]>  {
    return this.http.get<FloorResponseDTO[]>(`${this.API_URL}/api/floors/buildings?building=${$event}`, {reportProgress: b});
  }
}
