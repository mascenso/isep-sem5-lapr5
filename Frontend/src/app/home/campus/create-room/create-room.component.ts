import { Component } from '@angular/core';
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";
import { FormControl, FormGroup } from "@angular/forms";
import { Subscription } from "rxjs";
import { FloorResponseDTO } from "../../../../dto/floorDTO";
import { MatSnackBar } from "@angular/material/snack-bar";
import { RoomService } from "../../../services/room.service";
import { BridgeRequestDTO } from "../../../../dto/bridgeDTO";
import { RoomRequestDTO } from "../../../../dto/roomDTO";

@Component({
  selector: 'app-create-room',
  templateUrl: './create-room.component.html',
  styleUrls: ['./create-room.component.css']
})
export class CreateRoomComponent {

  buildingList: BuildingResponseDTO[] = [];
  buildingSelectionControl = new FormControl();
  buildingServiceSubscription$ = new Subscription();

  floorAList: FloorResponseDTO[] = [];
  floorASelectionControl = new FormControl();

  roomForm = new FormGroup({
    name: new FormControl(''),
    description: new FormControl(''),
    roomType: new FormControl(''),
    buildingId: new FormControl(''),
    floorId: new FormControl(''),
  });

  createdRoom: BridgeRequestDTO | undefined;


  constructor(private roomService: RoomService,
              private _snackBar: MatSnackBar) {
  }

  ngOnInit(): void {
    this.buildingServiceSubscription$ = this.roomService.getAllBuildings().subscribe(
      response => {
        this.buildingList = response;
      },

    )
  }

  onSelectionUpdateSelectionControlA($event: any) {
    if (!$event) {
      this.floorAList = [];
      return;
    }

    this.buildingServiceSubscription$ = this.roomService.getFloorsByBuildingId($event, true).subscribe(
      response => {
        this.floorAList = response;
      },
      error => {
        this._snackBar.open("Unable to get floors!", "close", {
          duration: 5000,
          panelClass: ['snackbar-warning']
        });
      }
    );

  }


  onSubmit() {
    this.roomForm.patchValue({
      floorId: this.floorASelectionControl.value,
      buildingId: this.buildingSelectionControl.value,
      name: this.roomForm.value.name,
      description: this.roomForm.value.description,
      roomType: this.roomForm.value.roomType
    });

    this.roomService.createRoom(this.roomForm.value as RoomRequestDTO, true).subscribe(
      response => {
        this.createdRoom = response;
        this._snackBar.open("Bridge created!", "close", {
          duration: 5000,
          panelClass: ['snackbar-success']
        });
      },
      (error) => {
        //Throwing error to global error handler
        throw error;
      }

    );
  }
}
