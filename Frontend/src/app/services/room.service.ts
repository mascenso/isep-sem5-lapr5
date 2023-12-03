import { Injectable } from "@angular/core";
import { catchError, forkJoin, map, Observable, switchMap, throwError } from "rxjs";
import { HttpClient, HttpErrorResponse } from "@angular/common/http";
import { environment } from "../../environments/environment";
import { BuildingResponseDTO } from "../../dto/buildingDTO";
import { FloorResponseDTO } from "../../dto/floorDTO";
import { retry } from "rxjs/operators";
import { RoomRequestDTO, RoomResponseDTO } from "../../dto/roomDTO";


@Injectable({
  providedIn: "root"
})
export class RoomService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) {
  }

    //{{baseUrl}}/api/buildings/:buildingId/floors/:floorId/rooms
  public createRoom(room: RoomRequestDTO, showSpinner?: boolean): Observable<RoomResponseDTO> {
    const url = `${this.API_URL}/api/buildings/${room.buildingId}/floors/${room.floorId}/rooms`;

    let roomRequest = {
      name : room.name,
      description: room.description,
      roomType: room.roomType
    };

    return this.http.post<RoomResponseDTO>(url, roomRequest as RoomResponseDTO, { reportProgress: showSpinner })
      .pipe(
        retry(1),
        catchError((error: HttpErrorResponse) => {
          console.log(roomRequest);
          return throwError(error);
        })
      );
  }

  getAllBuildings(): Observable<BuildingResponseDTO[]> {
    return this.http.get<BuildingResponseDTO[]>(`${this.API_URL}/api/buildings`);
  }

  getFloorsByBuildingId($event: any, b: boolean): Observable<FloorResponseDTO[]> {
    return this.http.get<FloorResponseDTO[]>(`${this.API_URL}/api/floors/buildings?building=${$event}`, { reportProgress: b });
  }


}
