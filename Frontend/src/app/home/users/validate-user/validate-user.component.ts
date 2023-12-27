import { animate, state, style, transition, trigger } from '@angular/animations';
import { Component, OnInit } from '@angular/core';
import { MatSnackBar } from '@angular/material/snack-bar';
import { Subscriber, Subscription } from 'rxjs';
import { UserService } from 'src/app/services/user.service';
import { UserResponseDTO } from 'src/dto/userDTO';

@Component({
  selector: 'app-validate-user',
  templateUrl: './validate-user.component.html',
  animations: [
    trigger('detailExpand', [
      state('collapsed,void', style({height: '0px', minHeight: '0'})),
      state('expanded', style({height: '*'})),
      transition('expanded <=> collapsed', animate('225ms cubic-bezier(0.4, 0.0, 0.2, 1)')),
    ]),
  ],
  styleUrls: ['./validate-user.component.css']
})
export class ValidateUserComponent implements OnInit {
  userList: UserResponseDTO[] = [];
  userServiceSubscription$ = new Subscription();


  columnsToDisplay = ['email', 'firstName', 'lastName'];
  columnsToDisplayWithExpand = [...this.columnsToDisplay, 'expand'];
  expandedElement: UserResponseDTO | null | undefined;

  constructor(private userService: UserService,
    private _snackBar: MatSnackBar) {}

    ngOnInit(): void {
      this.userServiceSubscription$ = this.userService.GetInactiveUsers().subscribe(
        response => {
          this.userList = response;
        },
        error => {
          this._snackBar.open("Unable to get users!", "close", {
            duration: 5000,
            panelClass: ['snackbar-warning']
          });
        }
      )
    }
}
