describe('ListElevatorsComponent', () => {
    beforeEach(() => {
      cy.request('http://localhost:4000/api/elevators'); 
    });

    it('should display a list of buildings', () => {
        cy.request('http://localhost:4000/api/buildings').then((response) => {
          expect(response.status).to.eq(200); // Ensure the request was successful
      
          const buildings = response.body;
          expect(buildings).to.have.length.greaterThan(0); // Ensure there are buildings
      
          // Espera um segundo antes de verificar se os elementos mat-option estão presentes
          //cy.wait(1000);
      
          //cy.get('mat-option').should('have.length', buildings.length); // Verifica se mat-option corresponde ao número de edifícios
        });
      });
      
    it('should update table when a building is selected', () => {
      // Assuming a building selection triggers an API call to get elevators
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
});
