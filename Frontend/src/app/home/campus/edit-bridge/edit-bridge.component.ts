import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from "@angular/forms";
import { BridgeService } from '../../../services/bridge.service';
import {MatSnackBar} from "@angular/material/snack-bar";
import { FloorDTO, FloorResponseDTO } from "../../../../dto/floorDTO";
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";
import { Subscription } from "rxjs";
import { BridgeRequestDTO, BridgeResponseDTO } from "../../../../dto/bridgeDTO";

@Component({
  selector: 'app-edit-bridge',
  templateUrl: './edit-bridge.component.html',
  styleUrls: ['./edit-bridge.component.css']
})
export class EditBridgeComponent {

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


  bridgeForm!: FormGroup;
  bridges: BridgeResponseDTO[] = [];

  floors: FloorDTO[] = [];
  buildings: BuildingResponseDTO[] = [];

  floorAId: string = '';
  floorBId: string = '';

  selectedBridge: any ;

  constructor(
    private formBuilder: FormBuilder,
    private bridgeService: BridgeService,
    private _snackBar: MatSnackBar
  ) {
    this.bridgeForm = this.formBuilder.group({
      code: [''],
      name: [''],
      floorAId: [null],
      floorBId: [null],
    });
  }

  ngOnInit(): void {
    this.getBridges();

    this.buildingServiceSubscription$ = this.bridgeService.getAllBuildings().subscribe(
      response => {
        this.buildingList = response;
        console.log(this.buildingList = response);
        this.buildingBList = response;
      },

    )


  }

  onBridgeSelected(bridgeId: string): void {
    this.selectedBridge = this.bridges.find(bridge => bridge.code === bridgeId)
    this.updateBridgeForm()
  }

  updateBridgeForm(){
    this.bridgeForm = this.formBuilder.group({
      code: [this.selectedBridge?.code || ''],
      name: [this.selectedBridge?.name || ''],
    });
  }

  getBridges(){
    this.bridgeService.getAllBridges().subscribe((bridges) => {
      console.log(bridges);
      this.bridges = bridges;
    });
  }

  onSubmit(): void {

    let updates = {
      code: this.selectedBridge.code,
      name: this.bridgeForm.value.name,
      floorAId: this.floorAId,
      floorBId: this.floorBId
    };

    this.bridgeService.editBridge(updates as BridgeRequestDTO, this.selectedBridge.id ).subscribe(
      (bridge) => {

          this.selectedBridge = bridge;
          this.updateBridgeForm();
          this.getBridges();

          this._snackBar.open("Bridge updated!", "close", {
            duration: 5000,
            panelClass: ['snackbar-success']
          });
      },
      (error) => {
        this._snackBar.open("Error in bridge update!", "close", {
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


  onSelectionIdentifyFloorIdB($event: any) {
    if (!$event) {
      this.floorBId = '';
      return;
    }
    this.floorBId = $event;
    console.log(this.floorBId);
  }

  onSelectionIdentifyFloorIdA($event: any) {
    if (!$event) {
      this.floorAId = '';
      return;
    }
    this.floorAId = $event;
    console.log(this.floorAId);
  }
}
