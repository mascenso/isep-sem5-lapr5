import { animate, state, style, transition, trigger } from '@angular/animations';
import { Component, OnDestroy, OnInit } from '@angular/core';
import { FormControl } from '@angular/forms';
import { MatSnackBar } from '@angular/material/snack-bar';
import {Observable, Subscription} from "rxjs";
import { BuildingService } from 'src/app/services/building.service';
import { ElevatorService } from 'src/app/services/elevator.service';
import { BuildingResponseDTO } from 'src/dto/buildingDTO';
import { ElevatorResponseDTO } from 'src/dto/elevatorDTO';


@Component({
  selector: 'app-list-elevators',
  templateUrl: './list-elevators.component.html',
  animations: [
    trigger('detailExpand', [
      state('collapsed,void', style({height: '0px', minHeight: '0'})),
      state('expanded', style({height: '*'})),
      transition('expanded <=> collapsed', animate('225ms cubic-bezier(0.4, 0.0, 0.2, 1)')),
    ]),
  ],
  styleUrls: ['./list-elevators.component.css']
})
export class ListElevatorsComponent implements OnInit, OnDestroy{
  buildingList: BuildingResponseDTO[] = [];
  buildingSelectionControl =  new FormControl();
  buildingServiceSubscription$ = new Subscription();

  elevatorServiceSubscription$ = new Subscription();

  dataSource: ElevatorResponseDTO[] = [];
  columnsToDisplay = ['buildingId', 'code', 'floorList'];
  columnsToDisplayWithExpand = [...this.columnsToDisplay, 'expand'];
  expandedElement: ElevatorResponseDTO | null | undefined;

  constructor(private buildingService: BuildingService,
              private elevatorService: ElevatorService,
              private _snackBar: MatSnackBar) {}


  ngOnInit(): void {
    // fetch building list from service
    this.buildingServiceSubscription$ = this.buildingService.getAllBuildings().subscribe(
      response => {
        this.buildingList = response;
      },
      error => {
        this._snackBar.open("Unable to get buildings!", "close", {
          duration: 5000,
          panelClass: ['snackbar-warning']
        });
      }
    )
  }

  onSelectionUpdateTable(selection: any): void {
    console.log(selection);
    if (selection) {
      this.elevatorServiceSubscription$ = this.elevatorService.getBuildingElevators(selection, true ).subscribe(
        elevatorData => {
          this.dataSource = [elevatorData];
          console.log(this.dataSource);
        },
        error => {
          this._snackBar.open("That building doesn't have an elevator!", "close", {
            duration: 5000,
            panelClass: ['snackbar-error']
          });
        }
      )
    }
  }

  ngOnDestroy(): void {
    this.buildingServiceSubscription$.unsubscribe();
    this.elevatorServiceSubscription$.unsubscribe();
  }

}
