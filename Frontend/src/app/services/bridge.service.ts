import { Injectable } from '@angular/core';
import {Observable} from "rxjs";
import {HttpClient} from "@angular/common/http";
import {environment} from "../../environments/environment";
import { BridgeResponseDTO, CreateBridgeRequestDTO } from "../../dto/bridgeDTO";
import { BuildingResponseDTO } from "../../dto/buildingDTO";
import { FloorResponseDTO } from "../../dto/floorDTO";


@Injectable({
  providedIn: 'root'
})
export class BridgeService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) { }

  public createBridge(bridge: CreateBridgeRequestDTO, showSpinner?: boolean): Observable<BridgeResponseDTO> {
    const url = `${this.API_URL}/api/bridges`;
    return this.http.post<BridgeResponseDTO>(url, bridge, {reportProgress: showSpinner});
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
