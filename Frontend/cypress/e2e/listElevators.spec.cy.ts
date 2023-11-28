describe('Teste para lista de Elevadores', () => {
    beforeEach(() => {
      //criar edificio para ver na tabela
      cy.visit('http://localhost:4200/login');
      cy.get('#role').select('Administrador');
      cy.get('#signUp').click();
      cy.get('[id^="Campus"]').click();

      //crio um building para garantir que tenho pelo menos um na dropdown do floor e do elevator
      cy.get('[routerlink="create-building"]').click();
      const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
      cy.get('#mat-input-0').type('A'+numeroAleatorio.toString());
      cy.get('#mat-input-1').type('10');
      cy.get('#mat-input-2').type('50');
      cy.get('#mat-input-3').type('Edificio A');
      cy.get('#mat-input-4').type('Edificio A - Administracao');
      cy.get('.mdc-button__label').click();

      //crio um floor no primeiro building da dropdown
      cy.get('[id^="Campus"]').click();
      cy.get('[routerlink="create-floor"]').click();
      cy.intercept('POST', 'http://localhost:4000/api/floors').as('rotaFloor');

      cy.get('#dropdownCreateFloor').click().get('#mat-option-0').click();

      cy.get('#mat-input-5').type("10");
      cy.get('#mat-input-6').type('10');
      cy.get('#mat-input-7').type('4');
      cy.get('#mat-input-8').type('Mais um floor de teste');
      cy.get('#saveNewFloor').click();


      cy.get('[id^="Campus"]').click();
      cy.get('[routerlink="create-elevator"]').click();
      cy.intercept('POST', 'http://localhost:4000/api/elevators').as('rotaElevator');

      cy.get('#mat-input-9').type('ELEV'+numeroAleatorio.toString());
      cy.get('#mat-select-value-3').click().get('#mat-option-3').click();
      cy.get('#mat-select-value-5').click().get('#mat-option-2').get('.mat-pseudo-checkbox').click();
      cy.get('.mdc-button__label').click();
/*
      cy.get('#dropdownCreateFloor').click().get('#mat-option-0').click();

      cy.get('#mat-input-9').type("");
      cy.get('#mat-input-6').type('10');
      cy.get('#mat-input-7').type('4');
      cy.get('#mat-input-8').type('Mais um floor de teste');
      cy.get('#saveNewFloor').click();


      cy.get('#mat-input-0').click().then(() => {
        cy.get('mat-option').last().click();
      });
      cy.get('#mat-input-1').type('10');
      cy.get('#mat-input-2').type('50');
      cy.get('#mat-input-3').type('1');
      cy.get('#mat-input-4').type('this is test');
      cy.get('.mdc-button__label').click();
      //ir para a pagina correta do teste
      cy.get('[id^="Campus"]').click();
      cy.get('[routerlink="list-floors"]').click();

  



  
      cy.get('.mat-mdc-form-field-infix').click();
      cy.get('#mat-option-0').first().click();

      cy.get('.mdc-button__label').click();
      //ir para a pagina correta do teste
      cy.get('[id^="Campus"]').click();
      cy.get('[routerlink="list-elevators"]').click();
 */
    });

    it('should display a list of buildings', () => {
        cy.request('http://localhost:4200/api/buildings').then((response) => {
          expect(response.status).to.eq(200); // Ensure the request was successful
      
          const buildings = response.body;
          expect(buildings).to.have.length.greaterThan(0); // Ensure there are buildings
      
          // Espera um segundo antes de verificar se os elementos mat-option estão presentes
          //cy.wait(1000);
      
          //cy.get('mat-option').should('have.length', buildings.length); // Verifica se mat-option corresponde ao número de edifícios
        });
      });

      /*
      it('Deve exibir a tabela corretamente e os valores da primeira coluna que vai buscar a bd', () => {

        //compara com o ultimo da tabela pois é esse que criamos
  
        cy.get('table.mat-elevation-z8').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-code').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-floorList').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-buildingID').should('exist');
        cy.get('.mdc-data-table__content > :last-child > .cdk-column-name').invoke('text').should('eq', ' Elev A ');
        cy.get('.mdc-data-table__content > :last-child > .cdk-column-description').invoke('text').should('eq', ' Edificio A - Administracao ');
        cy.get('.mdc-data-table__content > :last-child > .cdk-column-maxLength').invoke('text').should('eq', ' 50 ');
        cy.get('.mdc-data-table__content > :last-child > .cdk-column-maxWidth').invoke('text').should('eq', ' 10 ');
    });
      
    it('should update table when a building is selected', () => {
      // Assuming a building selection triggers an API call to get elevators
      cy.get('table.mat-elevation-z8').should('exist');
      cy.get('mat-select').select('Edificio C'); // Replace with your building select element class or ID
      cy.get('.elevator-table-row').should('have.length.greaterThan', 0); // Check if elevators are displayed in the table
    });
  
    it('should handle errors when fetching elevators', () => {
      // Mock a scenario where elevator service returns an error
      cy.intercept('GET', '/0b19edb1-678c-4c0d-b0e5-029a17b4cd05', {
        statusCode: 500,
        body: 'Error fetching elevators',
      }).as('getElevatorsError');
  
      cy.get('mat-select').select('Edificio A'); // Replace with a building that triggers an error
      cy.wait('@getElevatorsError');
      cy.get('.snackbar-error').should('be.visible'); // Check if error snackbar is displayed
    });
  
    it('should display elevators for a selected building', () => {
        // Mock successful elevator retrieval for a specific building
        cy.intercept('GET', '/elevators/6dccd2dc-8c9e-4ddc-924b-a32ed318bcb5', {
          statusCode: 200,
          body: {
            "id": "a726671b-e08a-4ea2-89e0-e44285314d7f",
            "code": "ElevA1-PUT",
            "floorList": [
              "760d06d2-560f-4506-8816-dae10ae6d735",
              "b6c66188-160c-4739-8ed6-a4874c23494b"
            ],
            "buildingId": "6dccd2dc-8c9e-4ddc-924b-a32ed318bcb5"
          },
        }).as('getBuildingElevators');
    
        cy.wait('@getBuildingElevators').its('response.statusCode').should('eq', 200);
       // cy.get('.elevator-table-row').should('have.length.greaterThan', 0);
      });
      */
});
