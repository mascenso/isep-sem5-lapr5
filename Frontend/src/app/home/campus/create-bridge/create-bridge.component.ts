import { Component } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {MatSnackBar} from "@angular/material/snack-bar";
import { Subscription } from "rxjs";
import { BridgeService } from "../../../services/bridge.service";
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";
import { FloorResponseDTO } from "../../../../dto/floorDTO";
import { BridgeRequestDTO } from "../../../../dto/bridgeDTO";

@Component({
  selector: 'app-create-bridge',
  templateUrl: './create-bridge.component.html',
  styleUrls: ['./create-bridge.component.css']
})
export class CreateBridgeComponent {

  buildingList: BuildingResponseDTO[] = [];
  buildingSelectionControl = new FormControl();
  buildingServiceSubscription$ = new Subscription();

  buildingBList: BuildingResponseDTO[] = [];
  buildingBSelectionControl = new FormControl();

  floorAList: FloorResponseDTO[] = [];
  floorASelectionControl = new FormControl();
  floorAServiceSubscription$ = new Subscription();

  floorBList: FloorResponseDTO[] = [];
  floorBSelectionControl = new FormControl();
  floorBServiceSubscription$ = new Subscription();


  bridgeForm = new FormGroup({
    //code: new FormControl('', [Validators.required]),
    name: new FormControl(''),
    floorAId: new FormControl(''),
    floorBId: new FormControl('')
  });


  ngOnInit(): void {
    this.buildingServiceSubscription$ = this.bridgeService.getAllBuildings().subscribe(
      response => {
        this.buildingList = response;
        console.log(this.buildingList = response);
        this.buildingBList = response;
      },

    )
  }

  createdBridge: BridgeRequestDTO | undefined;

  constructor(private bridgeService: BridgeService,
              private _snackBar: MatSnackBar) {
  }

  public onSubmit() {
    this.bridgeForm.patchValue({
      floorAId: this.floorASelectionControl.value,
      floorBId: this.floorBSelectionControl.value
    });

    this.bridgeService.createBridge(this.bridgeForm.value as BridgeRequestDTO, true).subscribe(
      response => {
        this.createdBridge = response;
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

  onSelectionUpdateSelectionControlA($event: any) {
    if (!$event) {
      this.floorAList = [];
      return;
    }

    this.floorBServiceSubscription$ = this.bridgeService.getFloorsByBuildingId($event, true).subscribe(
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

  onSelectionUpdateSelectionControlB($event: any) {
    if (!$event) {
      this.floorBList = [];
      return;
    }
    this.floorAServiceSubscription$ = this.bridgeService.getFloorsByBuildingId($event, true).subscribe(
      response => {
        this.floorBList = response;
      },
      error => {
        this._snackBar.open("Unable to get floors!", "close", {
          duration: 5000,
          panelClass: ['snackbar-warning']
        });
      }
    );

  }

}



