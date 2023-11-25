import { Injectable } from "@angular/core";
import { catchError, forkJoin, map, Observable, switchMap, throwError } from "rxjs";
import { HttpClient, HttpErrorResponse } from "@angular/common/http";
import { environment } from "../../environments/environment";
import { BridgeDTO, BridgeFloorBuildingDTO, BridgeResponseDTO, CreateBridgeRequestDTO } from "../../dto/bridgeDTO";
import { BuildingResponseDTO } from "../../dto/buildingDTO";
import { FloorResponseDTO } from "../../dto/floorDTO";
import { retry } from "rxjs/operators";


@Injectable({
  providedIn: "root"
})
export class BridgeService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) {
  }

  public createBridge(bridge: CreateBridgeRequestDTO, showSpinner?: boolean): Observable<BridgeResponseDTO> {
    const url = `${this.API_URL}/api/bridges`;
    return this.http.post<BridgeResponseDTO>(url, bridge, { reportProgress: showSpinner })
      .pipe(
        retry(1),
        catchError((error: HttpErrorResponse) => {
          return throwError(error);
        })
      );
  }

  getAllBridges(): Observable<BridgeFloorBuildingDTO[]> {
    return this.http.get<BridgeDTO[]>(`${this.API_URL}/api/bridges`).pipe(
      switchMap((bridges) =>
        forkJoin(
          bridges.map((bridge) =>
            this.fetchFloorAndBuildingData(bridge.floorAId, bridge.buildingAId, bridge.floorBId, bridge.buildingBId).pipe(
              map((data) => ({
                code: bridge.code,
                name: bridge.name,
                floorNumberA: data.floorNumberA,
                buildingNameA: data.buildingNameA,
                floorNumberB: data.floorNumberB,
                buildingNameB: data.buildingNameB
              }))
            )
          )
        )
      )
    );

  }


  private fetchFloorAndBuildingData(floorIdA: string, buildingIdA: string, floorIdB: string, buildingIdB: string): Observable<any> {
    return forkJoin([
      this.http.get<FloorResponseDTO>(`${this.API_URL}/api/floors/${floorIdA}`),
      this.http.get<BuildingResponseDTO>(`${this.API_URL}/api/buildings/${buildingIdA}`),
      this.http.get<FloorResponseDTO>(`${this.API_URL}/api/floors/${floorIdB}`),
      this.http.get<BuildingResponseDTO>(`${this.API_URL}/api/buildings/${buildingIdB}`)
    ]).pipe(
      map(([floorA, buildingA, floorB, buildingB]) => ({
        floorNumberA: floorA.floorNumber,
        buildingNameA: buildingA.name,
        floorNumberB: floorB.floorNumber,
        buildingNameB: buildingB.name
      }))
    );
  }

  getAllBuildings(): Observable<BuildingResponseDTO[]> {
    return this.http.get<BuildingResponseDTO[]>(`${this.API_URL}/api/buildings`);
  }

  getFloorsByBuildingId($event: any, b: boolean): Observable<FloorResponseDTO[]> {
    return this.http.get<FloorResponseDTO[]>(`${this.API_URL}/api/floors/buildings?building=${$event}`, { reportProgress: b });
  }
}
