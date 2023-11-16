import { Component } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {
  BridgeResponseDto,
  BridgeService,
  CreateBridgeRequestDto,
  FloorResponseDto
} from "../../../services/bridge.service";
import {MatSnackBar} from "@angular/material/snack-bar";
import { BuildingResponseDto } from "../../../services/building.service";
import { Subscription } from "rxjs";

@Component({
  selector: 'app-create-bridge',
  templateUrl: './create-bridge.component.html',
  styleUrls: ['./create-bridge.component.css']
})
export class CreateBridgeComponent {

  buildingList: BuildingResponseDto[] = [];
  buildingSelectionControl = new FormControl();
  buildingServiceSubscription$ = new Subscription();

  buildingBList: BuildingResponseDto[] = [];
  buildingBSelectionControl = new FormControl();

  floorAList: FloorResponseDto[] = [];
  floorASelectionControl = new FormControl();
  floorAServiceSubscription$ = new Subscription();

  floorBList: FloorResponseDto[] = [];
  floorBSelectionControl = new FormControl();
  floorBServiceSubscription$ = new Subscription();

  ngOnInit(): void {
    this.buildingServiceSubscription$ = this.bridgeService.getAllBuildings().subscribe(
      response => {
        this.buildingList = response;
        this.buildingBList = response;
      },
      error => {
        this._snackBar.open("Unable to get buildings!", "close", {
          duration: 5000,
          panelClass: ['snackbar-warning']
        });
      }
    )
  }

  bridgeForm = new FormGroup({
    code: new FormControl('', [Validators.required]),
    name: new FormControl(''),
    floorAId: new FormControl('', [Validators.required]),
    floorBId: new FormControl('', [Validators.required])
  });

  createdBridge: BridgeResponseDto | undefined;


  constructor(private bridgeService: BridgeService,
              private _snackBar: MatSnackBar) {
  }



  public onSubmit() {

    const selectedFloorA = this.floorASelectionControl.value;
    const selectedFloorB = this.floorBSelectionControl.value;

    this.bridgeForm.patchValue({
      floorAId: selectedFloorA,
      floorBId: selectedFloorB
    });


    console.log(selectedFloorA);


console.log(this.bridgeForm.value);
    this.bridgeService.createBridge(this.bridgeForm.value as CreateBridgeRequestDto, true).subscribe(
      response => {
        this.createdBridge = response;
        this._snackBar.open("Bridge created!", "close", {
          duration: 5000,
          panelClass: ['snackbar-success']
        });
      },
      error => {
        console.log('Error creating bridge: ', error);
        this._snackBar.open(error.message, "close", {
          duration: 5000,
          panelClass: ['snackbar-error']
        });
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



