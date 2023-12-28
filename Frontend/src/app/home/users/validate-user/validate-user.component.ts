import { animate, state, style, transition, trigger } from '@angular/animations';
import { Component, OnInit } from '@angular/core';
import { MatSnackBar } from '@angular/material/snack-bar';
import { Router } from '@angular/router';
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
  expandedElement: any;

  constructor(private userService: UserService,
    private router: Router,
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

    toggleRowExpansion(row: any): void {
      this.expandedElement = this.expandedElement === row ? null : row;
    }

    acceptUser(user: UserResponseDTO): void {
      let approvedUser = {
        id: user.id,
        email: user.email,
        firstName: user.firstName,
        lastName: user.lastName,
        role: user.role,
        active: true,
      };

      console.log('Accepted user:', approvedUser);

      this.userService.updateUser(user.id, approvedUser as UserResponseDTO).subscribe(
        (approvedUser) => {
          this._snackBar.open("User approved!", "close", {
            duration: 5000,
            panelClass: ['snackbar-success']
          });
        },
        (error) => {
          this._snackBar.open("Error in user approval!", "close", {
            duration: 5000,
            panelClass: ['snackbar-error']
          });
        }
      );
      console.log(user.id);
      console.log(approvedUser);
    
      // Logic to handle user acceptance
      // Example: Make an API call to accept the user
    }
    
  
    rejectUser(user: UserResponseDTO): void {
      this.userService.deleteUser(user.id).subscribe(
        response => {this._snackBar.open("User deleted!", "close", {
          duration: 5000,
          panelClass: ['snackbar-success']
        });
        this.router.navigate(['/home/users']);
      },
      (error) => {
        this._snackBar.open("Error in user rejection!", "close", {
          duration: 5000,
          panelClass: ['snackbar-error']
        });
      }
    );
      // Logic to handle user rejection
      // Example: Make an API call to reject the user
      console.log('Rejected user:', user);
    }

    
}
