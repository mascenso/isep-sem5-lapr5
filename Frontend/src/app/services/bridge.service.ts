import { Injectable } from "@angular/core";
import { catchError, forkJoin, map, Observable, switchMap, throwError } from "rxjs";
import { HttpClient, HttpErrorResponse } from "@angular/common/http";
import { environment } from "../../environments/environment";
import { BuildingResponseDTO } from "../../dto/buildingDTO";
import { FloorResponseDTO } from "../../dto/floorDTO";
import { retry } from "rxjs/operators";
import { BridgeRequestDTO, BridgeResponseDTO } from "../../dto/bridgeDTO";


@Injectable({
  providedIn: "root"
})
export class BridgeService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) {
  }

  public createBridge(bridge: BridgeRequestDTO, showSpinner?: boolean): Observable<BridgeRequestDTO> {
    const url = `${this.API_URL}/api/bridges`;
    return this.http.post<BridgeRequestDTO>(url, bridge, { reportProgress: showSpinner })
      .pipe(
        retry(1),
        catchError((error: HttpErrorResponse) => {
          return throwError(error);
        })
      );
  }

  getAllBridges() : Observable<BridgeResponseDTO[]> {
    return this.http.get<BridgeResponseDTO[]>(`${this.API_URL}/api/bridges`);
  }


  getAllBuildings(): Observable<BuildingResponseDTO[]> {
    return this.http.get<BuildingResponseDTO[]>(`${this.API_URL}/api/buildings`);
  }

  getFloorsByBuildingId($event: any, b: boolean): Observable<FloorResponseDTO[]> {
    return this.http.get<FloorResponseDTO[]>(`${this.API_URL}/api/floors/buildings?building=${$event}`, { reportProgress: b });
  }

  public editBridge(bridge:  BridgeRequestDTO, id: string ): Observable<BridgeResponseDTO> {
    console.log(bridge);
    return this.http.put<BridgeResponseDTO>(`${this.API_URL}/api/bridges/${id}`,bridge);
  }
}
